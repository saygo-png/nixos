{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Launcher for an shh-based GHCi shell.
--
-- Conventions:
--
--   * 'OsPath'      – every filesystem path. Literals via @[osp|...|]@.
--   * 'PosixString' – argv and environment, as the unix package passes them.
--                     Literals via @[pstr|...|]@. On POSIX an 'OsPath' is a
--                     'PosixString' underneath, so 'getOsString' converts a
--                     path for free.
--   * 'ByteString'  – file contents.
--   * 'Text'        – human-readable content, turned into bytes with UTF-8
--                     ('encodeUtf8'), never with the locale.
--   * 'String'      – only where an API insists ('getArgs', 'die').
module Main (main) where

import Data.List (partition)
import Language.Haskell.TH.Syntax qualified as TH
import Relude
import Relude.Unsafe (fromJust)
import System.Directory.OsPath (
  XdgDirectory (XdgCache, XdgConfig),
  createDirectoryIfMissing,
  doesFileExist,
  getXdgDirectory,
  removeFile,
 )
import System.Environment (getExecutablePath)
import System.File.OsPath qualified as File
import System.OsPath (OsPath, decodeFS, encodeFS, osp, takeDirectory, unsafeEncodeUtf, (</>))
import System.OsString.Internal.Types (OsString (getOsString))
import System.OsString.Posix (pstr)
import System.Posix.Env.PosixString qualified as Env
import System.Posix.Process.PosixString (executeFile)
import System.Posix.Resource (Resource (ResourceOpenFiles), ResourceLimit (..), ResourceLimits (softLimit), getResourceLimit, setResourceLimit)

-------------------------------------------------------------------------------
-- Get paths from envvars set during the Nix build.

ghcEnv :: OsPath
ghcEnv = unsafeEncodeUtf $ fromJust $(TH.lift =<< TH.runIO (lookupEnv "SHHELL_GHC_ENV"))

ghcBin, ghcExe, ghciExe :: OsPath
ghcBin = ghcEnv </> [osp|bin|]
ghcExe = ghcBin </> [osp|ghc|]
-- GHCi with a newer haskeline that reads the XDG config, see ghci.nix.
ghciExe = unsafeEncodeUtf $ fromJust $(TH.lift =<< TH.runIO (lookupEnv "SHHELL_GHCI"))

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
    , "import Shhell.Prompt"
    , -- Also loads or unloads the current directory's .envrc, see Shhell.Prompt.
      ":set prompt-function shhellPrompt"
    , ":set prompt-cont \"| \""
    ]

-- | Cradle for haskell-language-server when editing Shell.hs (e.g. via @:e@).
-- A direct cradle asks the @ghc@ on PATH for its package database, and 'main'
-- puts 'ghcEnv' first on PATH, so the language server sees the same packages
-- as GHCi.
defaultHieYaml :: Text
defaultHieYaml =
  unlines
    [ "cradle:"
    , "  direct:"
    , "    arguments: []"
    ]

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

writeIfMissing :: OsPath -> Text -> IO ()
writeIfMissing p contents =
  unlessM (doesFileExist p) $ File.writeFile' p (encodeUtf8 contents)

-------------------------------------------------------------------------------

main :: IO ()
main = do
  Opts{optDebug, optMode} <- either (die . toString) pure . parseArgs . map toText =<< getArgs

  when (optMode == Help) $ putTextLn usage >> exitSuccess

  cfgDir <- getXdgDirectory XdgConfig [osp|shhell|]
  cacheDir <- getXdgDirectory XdgCache [osp|shhell|]

  let
    initGhci = cfgDir </> [osp|init.ghci|]
    shellHs = cfgDir </> [osp|Shell.hs|]
    hieYaml = cfgDir </> [osp|hie.yaml|]
    display = fmap toText . decodeFS

  when optDebug
    $ for_ [("Shh home", cfgDir), ("Build cache", cacheDir), ("ghc", ghcExe), ("ghci", ghciExe)]
    $ \(label, p) ->
      putTextLn . ((label <> ": ") <>) =<< display p

  createDirectoryIfMissing True cfgDir
  createDirectoryIfMissing True cacheDir

  unlessM (doesFileExist initGhci) $ do
    putTextLn "Generating init.ghci..."
    File.writeFile' initGhci . encodeUtf8 $ defaultInitGhci

  writeIfMissing shellHs defaultShell
  writeIfMissing hieYaml defaultHieYaml

  when (optMode == Rebuild)
    . for_ [[osp|Shell.o|], [osp|Shell.hi|]]
    $ liftA2 whenM doesFileExist removeFile
    . (cacheDir </>)

  -- Programs started from GHCi, like the editor and its language server on :e,
  -- inherit this PATH and so find the GHC that has shh and friends installed.
  path <-
    Env.getEnv [pstr|PATH|] >>= \case
      Just x -> pure $ [pstr|:|] <> x
      Nothing -> pure mempty
  Env.setEnv [pstr|PATH|] (getOsString ghcBin <> path) True

  -- Before running a program, shh's process library closes every file
  -- descriptor up to the soft limit on open files, one at a time. nix run and
  -- nix develop raise that limit to the hard one, often 524288, which makes
  -- each command about 100 ms slower. So lower it to a login session's default.
  -- Programs that need more can raise it again, up to the unchanged hard limit.
  limits <- getResourceLimit ResourceOpenFiles
  let tooHigh = case limits.softLimit of
        ResourceLimit n -> n > 1024
        ResourceLimitInfinity -> True
        ResourceLimitUnknown -> False
  when tooHigh $ setResourceLimit ResourceOpenFiles limits{softLimit = ResourceLimit 1024}

  -- GHCi links Shell.o into a shared library before it can run it, which it
  -- does when the first command is typed, and normally on every launch.
  -- ghc-link-cache, installed next to this program, stands in for the linker
  -- and keeps those libraries. -pgml turns off -no-pie for executables
  -- (GHC #15319), but ghc-link-cache runs GHC's own C compiler, which supports it.
  linkCache <- (</> [osp|ghc-link-cache|]) . takeDirectory <$> (encodeFS =<< getExecutablePath)

  -- executeFile replaces this process without flushing its handles.
  hFlush stdout

  -- argv[0] is set by executeFile; the arguments start after it.
  let
    -- The non-moving garbage collector collects the old generation while GHCi
    -- runs, instead of stopping it to copy all of it. Given as arguments, since
    -- every program started from the shell would inherit GHCRTS.
    rtsArgs = [[pstr|+RTS|], [pstr|-xn|], [pstr|-RTS|]]
    fileArgs = [[pstr|-outputdir|], getOsString cacheDir, [pstr|-ghci-script|], getOsString initGhci, getOsString shellHs]
    linkArgs = [[pstr|-pgml|], getOsString linkCache, [pstr|-pgml-supports-no-pie|]]
    quiet = [[pstr|-v0|] | not optDebug]

  executeFile (getOsString ghciExe) False (rtsArgs <> [[pstr|-fobject-code|]] <> fileArgs <> linkArgs <> quiet) Nothing
