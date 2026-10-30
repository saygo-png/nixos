{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A linker for GHCi to use (@-pgml ghc-link-cache@) that keeps the shared
-- libraries GHCi links object code into, and hands back a kept one instead of
-- linking again when nothing that goes into the link has changed.
--
-- A dynamically linked GHCi can only load shared libraries, so before it can
-- run code from an object file it links it into a temporary library. GHC
-- deletes that library at the end of the session, so every session links
-- again (about 250 ms for shhell's Shell.o). This program sits where GHC runs
-- the C compiler as its linker and caches the result.
--
-- A kept library is named after a hash of everything the link depends on, so
-- one with the right name is what a new link would produce. Loading a library
-- runs its code, so one is only used if nobody but us can have written it.
-- Anything unexpected falls back to linking as usual.
--
-- Conventions:
--
--   * 'OsPath'      – every filesystem path. Literals via @[osp|...|]@.
--   * 'PosixString' – argv and environment, as the unix package passes them.
--                     On POSIX an 'OsPath' is a 'PosixString' underneath, so
--                     'OsString' and 'getOsString' convert for free.
--   * 'ByteString'  – file contents, and the bytes that go into the hash.
--   * 'String'      – only where an API insists (ghc --info at build time).
module Main (main) where

import Control.Exception (IOException, catch, try)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.List qualified as L
import Data.Time.Clock (addUTCTime, getCurrentTime, nominalDay)
import Language.Haskell.TH.Syntax qualified as TH
import Relude
import System.Directory.OsPath (
  XdgDirectory (XdgCache),
  copyFile,
  createDirectoryIfMissing,
  doesFileExist,
  getModificationTime,
  getXdgDirectory,
  listDirectory,
  removeFile,
  setModificationTime,
 )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.File.OsPath qualified as File
import System.IO.Error (ioError, userError)
import System.OsPath (OsPath, osp, takeDirectory, (</>))
import System.OsString.Internal.Types (OsString (OsString, getOsString), PosixString (getPosixString))
import System.OsString.Posix (fromBytestring, pstr)
import System.OsString.Posix qualified as PS
import System.Posix.Directory.PosixPath (createDirectory)
import System.Posix.Env.PosixString qualified as Env
import System.Posix.Files.PosixString qualified as Posix
import System.Posix.Process.PosixString (ProcessStatus (Exited), executeFile, forkProcess, getProcessStatus)
import System.Posix.User (getEffectiveUserID)
import System.Process (readProcess)

-------------------------------------------------------------------------------
-- The real linker

-- | The program GHC links with and the options it passes before its own
-- arguments (GHC's @pgm_l@), taken from the @ghc@ in SHHELL_GHC_ENV at build
-- time. @-pgml@ replaces the program and drops these options, so they are
-- added back here.
realLinker :: (PosixString, [PosixString])
realLinker =
  bimap fromBytestring (map fromBytestring) $
    $( do
        env <- TH.runIO (lookupEnv "SHHELL_GHC_ENV") >>= maybe (fail "SHHELL_GHC_ENV is not set") pure
        info <- TH.runIO (readProcess (env <> "/bin/ghc") ["--info"] "")
        fields <- maybe (fail "cannot parse ghc --info") pure (readMaybe info :: Maybe [(String, String)])
        let field k = maybe (fail ("ghc --info has no " <> show k)) pure (L.lookup k fields)
            bytes = encodeUtf8 . toText :: String -> ByteString
        cc <- field "C compiler command"
        ccFlags <- field "C compiler flags"
        linkFlags <- field "C compiler link flags"
        TH.lift (bytes cc, map bytes (L.words ccFlags <> L.words linkFlags))
     )

-- | Run the real linker and wait for it.
link :: [PosixString] -> IO ExitCode
link args = do
  pid <- forkProcess (execLinker args)
  getProcessStatus True False pid <&> \case
    Just (Exited code) -> code
    _ -> ExitFailure 1

-- | Replace this process with the real linker.
execLinker :: [PosixString] -> IO a
execLinker args = executeFile (fst realLinker) False (snd realLinker <> args) Nothing

-------------------------------------------------------------------------------
-- Arguments

-- | Expand @\@file@ arguments, which GHC uses to pass the arguments in a
-- response file: one argument per line, in double quotes, with backslash,
-- newline and double quote escaped.
expandArgs :: [PosixString] -> IO [PosixString]
expandArgs = fmap concat . traverse expand
  where
    expand arg
      | Just file <- PS.stripPrefix [pstr|@|] arg =
          ifM
            (doesFileExist (OsString file))
            (traverse unquote . filter (not . BS8.null) . BS8.lines =<< File.readFile' (OsString file))
            (pure [arg])
      | otherwise = pure [arg]
    unquote line
      | Just inner <- BS8.stripPrefix "\"" line >>= BS8.stripSuffix "\"" = pure (fromBytestring (unescape inner))
      | otherwise = ioError (userError "unexpected response file line")
    unescape s = case BS8.break (== '\\') s of
      (plain, rest) -> case BS8.uncons (BS8.drop 1 rest) of
        Nothing -> plain
        Just ('n', more) -> plain <> "\n" <> unescape more
        Just (c, more) -> plain <> BS8.singleton c <> unescape more

-- | A link GHCi makes to load object code: a shared library with a soname.
-- Returns the output file and the soname argument.
ghciLink :: [PosixString] -> Maybe (PosixString, PosixString)
ghciLink args = do
  guard ([pstr|-shared|] `elem` args)
  output <- outputOf args
  guard ([pstr|.so|] `PS.isSuffixOf` output)
  soname <- find ([pstr|-Wl,-h,|] `PS.isPrefixOf`) args
  pure (output, soname)
  where
    outputOf (o : out : rest) = if o == [pstr|-o|] then Just out else outputOf (out : rest)
    outputOf _ = Nothing

-------------------------------------------------------------------------------
-- Cache

-- | Hex SHA-256 of everything the link depends on: the linker and its
-- options, the arguments with the output named generically, the contents of
-- every input file, and the environment variables the C compiler reads.
cacheKey :: PosixString -> PosixString -> [PosixString] -> IO PosixString
cacheKey output soname args = do
  let normalised = map (\a -> if a == output then [pstr|<output>|] else if a == soname then [pstr|<soname>|] else a) args
      inputs = filter (\a -> a /= output && not ([pstr|-|] `PS.isPrefixOf` a)) args
  contents <- catMaybes <$> traverse readIfFile inputs
  env <- filter (relevantVar . fst) <$> Env.getEnvironment
  let strings = fst realLinker : snd realLinker <> normalised <> concatMap (\(k, v) -> [k, v]) env
      fields = map (fromShort . getPosixString) strings <> contents
      -- Separate the fields with NUL, which no argument or path can contain.
      digest = SHA256.finalize (foldl' SHA256.update SHA256.init (concatMap (\f -> [f, "\0"]) fields))
  pure (fromBytestring (toStrict (toLazyByteString (byteStringHex digest))))
  where
    readIfFile f = ifM (doesFileExist (OsString f)) (Just <$> File.readFile' (OsString f)) (pure Nothing)
    relevantVar v = [pstr|NIX_|] `PS.isPrefixOf` v || v `elem` [[pstr|LIBRARY_PATH|], [pstr|COMPILER_PATH|], [pstr|GCC_EXEC_PREFIX|]]

-- | Where kept libraries live: GHC_LINK_CACHE_DIR, or the XDG cache directory.
-- Created only accessible to us; Nothing if it is not private (see 'isPrivate').
cacheDir :: IO (Maybe OsPath)
cacheDir = do
  dir <- maybe (getXdgDirectory XdgCache [osp|ghc-link-cache|]) (pure . OsString) =<< Env.getEnv [pstr|GHC_LINK_CACHE_DIR|]
  unlessM (Posix.fileExist (getOsString dir)) $ do
    createDirectoryIfMissing True (takeDirectory dir)
    createDirectory (getOsString dir) Posix.ownerModes `catch` \(_ :: IOException) -> pass
  ifM (isPrivate True dir) (pure (Just dir)) (pure Nothing)

-- | Whether a path is a directory (True) or a regular file (False) owned by us
-- that nobody else can write to. A file must not be a symbolic link.
isPrivate :: Bool -> OsPath -> IO Bool
isPrivate isDir path = do
  let stat = if isDir then Posix.getFileStatus else Posix.getSymbolicLinkStatus
  status <- try (stat (getOsString path))
  uid <- getEffectiveUserID
  let othersWrite = Posix.groupWriteMode `Posix.unionFileModes` Posix.otherWriteMode
  pure $ case status of
    Left (_ :: IOException) -> False
    Right st ->
      Posix.fileOwner st == uid
        && (if isDir then Posix.isDirectory st else Posix.isRegularFile st)
        && Posix.fileMode st `Posix.intersectFileModes` othersWrite == Posix.nullFileMode

-- | Remove kept libraries not used for 30 days. Using one updates its time.
prune :: OsPath -> IO ()
prune dir = do
  cutoff <- addUTCTime (-30 * nominalDay) <$> getCurrentTime
  kept <- filter (PS.isPrefixOf [pstr|libghc_|] . getOsString) <$> listDirectory dir
  for_ kept $ \f ->
    whenM ((< cutoff) <$> getModificationTime (dir </> f)) $ removeFile (dir </> f)

-------------------------------------------------------------------------------

main :: IO ()
main = do
  raw <- Env.getArgs
  -- If anything about caching goes wrong, just link. The C compiler reads
  -- response files itself, so the original arguments are passed on.
  exitWith =<< (cachedLink raw `catch` \(_ :: IOException) -> execLinker raw)

cachedLink :: [PosixString] -> IO ExitCode
cachedLink raw = do
  args <- expandArgs raw
  dir <- cacheDir
  case (ghciLink args, dir) of
    (Just (output, soname), Just d) -> do
      key <- cacheKey output soname args
      let name = [pstr|libghc_|] <> key <> [pstr|.so|]
          kept = d </> OsString name
      ifM
        (isPrivate False kept)
        (copyFile kept (OsString output) >> getCurrentTime >>= setModificationTime kept >> pure ExitSuccess)
        $ do
          -- The soname is derived from the key too, so it is unique to this
          -- content and cannot clash with another library loaded in a session.
          code <- link (map (\a -> if a == soname then [pstr|-Wl,-h,|] <> name else a) args)
          when (code == ExitSuccess) $ do
            -- copyFile replaces the destination atomically, so concurrent
            -- sessions never see a partly written library.
            void (try @IOException (copyFile (OsString output) kept))
            void (try @IOException (prune d))
          pure code
    _ -> execLinker raw
