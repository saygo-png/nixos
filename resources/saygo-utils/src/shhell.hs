{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Launcher for an shh-based GHCi shell.
--
-- Conventions:
--
--   * 'OsPath'     – every filesystem path. Literals via @[osp|...|]@.
--   * 'ByteString' – file contents and argv. Paths become argv with 'osBytes'.
--   * 'Text'       – human-readable content, turned into bytes with UTF-8
--                    ('encodeUtf8'), never with the locale.
--   * 'String'     – only where an API insists ('getArgs', 'die').
module Main (main) where

import Data.ByteString qualified as BS
import Data.List (partition)
import Language.Haskell.TH.Syntax qualified as TH
import Relude
import Relude.Unsafe (fromJust)
import Shh (exe)
import System.Directory.OsPath (
  XdgDirectory (XdgConfig),
  createDirectoryIfMissing,
  doesFileExist,
  getXdgDirectory,
 )
import System.File.OsPath qualified as File
import System.OsPath (OsPath, decodeFS, osp, toChar, unpack, unsafeEncodeUtf, (</>))
import System.Posix.Process.ByteString (executeFile)

-------------------------------------------------------------------------------
-- GHC environment

ghcEnv :: OsPath
ghcEnv = unsafeEncodeUtf $ fromJust $(TH.lift =<< TH.runIO (lookupEnv "SHHELL_GHC_ENV"))

ghcBin, ghciBin :: OsPath
ghcBin = ghcEnv </> [osp|bin|] </> [osp|ghc|]
ghciBin = ghcEnv </> [osp|bin|] </> [osp|ghci|]

-------------------------------------------------------------------------------

-- Generated files

defaultShell :: Text
defaultShell =
  unlines
    [ "{-# LANGUAGE TemplateHaskell #-}"
    , "module Shell where"
    , "import Shh"
    , "$(loadEnv SearchPath)"
    ]

defaultInitGhci :: Text
defaultInitGhci =
  unlines
    [ ":seti -XNoOverloadedLists"
    , "import Shh"
    , "import Shh.Prompt"
    , ":set prompt-function formatPrompt " <> prompt
    , ":set prompt-cont \"| \""
    ]
  where
    prompt :: Text = "\"\\n\\ESC[1;32m[%u@%h:%w] λ \\ESC[0m\""

-------------------------------------------------------------------------------
-- Command line

data Mode = Run | Rebuild | Help
  deriving stock (Eq)

data Opts = Opts {optDebug :: Bool, optMode :: Mode}

parseArgs :: [Text] -> Either Text Opts
parseArgs args = Opts (not (null debugFlags)) <$> mode
  where
    (debugFlags, rest) = partition (== "--debug") args
    mode = case rest of
      [] -> Right Run
      ["--rebuild"] -> Right Rebuild
      ["--help"] -> Right Help
      _ -> Left ("Unknown arguments: " <> unwords rest <> "\n" <> usage)

usage :: Text
usage = "usage: shh [--rebuild] [--debug] [--help]"

-------------------------------------------------------------------------------
-- Helpers

-- | Exact bytes of a path, for argv. filepath has no public @toBytes@, but on
-- POSIX an 'OsChar' is one byte and 'toChar' maps it to the code point of the
-- same value, so this is lossless. POSIX-only, as is 'executeFile'.
osBytes :: OsPath -> ByteString
osBytes = BS.pack . map (fromIntegral . ord . toChar) . unpack

writeIfMissing :: OsPath -> Text -> IO ()
writeIfMissing p contents =
  unlessM (doesFileExist p) $ File.writeFile' p (encodeUtf8 contents)

-------------------------------------------------------------------------------

main :: IO ()
main = do
  Opts{optDebug, optMode} <- either (die . toString) pure . parseArgs . map toText =<< getArgs

  when (optMode == Help) $ putTextLn usage >> exitSuccess

  cfgDir <- getXdgDirectory XdgConfig [osp|shhell|]

  let
    initGhci = cfgDir </> [osp|init.ghci|]
    shellHs = cfgDir </> [osp|Shell.hs|]
    quiet = ["-v0" | not optDebug]
    display = fmap toText . decodeFS

  when optDebug
    $ for_ [("Shh home", cfgDir), ("ghc", ghcBin), ("ghci", ghciBin)]
    $ \(label, p) ->
      putTextLn . ((label <> ": ") <>) =<< display p

  createDirectoryIfMissing True cfgDir

  unlessM (doesFileExist initGhci) $ do
    putTextLn "Generating init.ghci..."
    File.writeFile' initGhci . encodeUtf8 $ defaultInitGhci

  writeIfMissing shellHs defaultShell

  exe (osBytes ghcBin) $ ["--make", "-no-link", "-dynamic", osBytes shellHs] <> ["-fforce-recomp" | optMode == Rebuild]

  -- argv[0] is set by executeFile; the arguments start after it.
  executeFile
    (osBytes ghciBin)
    False
    (["-ghci-script", osBytes initGhci, osBytes shellHs] <> quiet)
    Nothing
