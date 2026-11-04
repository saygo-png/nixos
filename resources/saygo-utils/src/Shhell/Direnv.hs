{-# LANGUAGE QuasiQuotes #-}

module Shhell.Direnv (direnvSync, direnvPrompt) where

import Control.Exception (IOException, try)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map.Strict qualified as Map
import Relude
import Shh (capture, exe, ignoreFailure, (|>))
import System.Directory.OsPath (withCurrentDirectory)
import System.OsPath (osp)
import System.OsString.Internal.Types (OsString (OsString), PosixString (PosixString))
import System.OsString.Posix (pstr)
import System.Posix.Env.PosixString qualified as Env

-- | Runs direnv with these arguments and returns what it printed.
runDirenv :: [ByteString] -> IO (Maybe LByteString)
runDirenv args = do
  -- GHCi runs the prompt in the directory it started in, not the one you cd'd
  -- to. shh's cd keeps $PWD up to date, so direnv runs there.
  let pwd = [pstr|PWD|]
      currentDir = [osp|.|]
  dir <-
    Env.getEnv pwd >>= \case
      Nothing -> pure currentDir
      Just r -> pure $ OsString r

  -- direnv prints its own messages, e.g. that an .envrc is blocked. It exits
  -- with an error then, but its output still has to be applied: leaving a
  -- loaded directory for a blocked one unloads the first. Without direnv, or
  -- if the directory is gone, there's no output.
  rightToMaybe <$> try @IOException (withCurrentDirectory dir $ ignoreFailure (exe ("direnv" : args)) |> capture)

direnvSync :: IO ()
direnvSync = do
  out <- runDirenv ["export", "json"]

  -- Changes to apply reported from direnv. A string to set a variable or null to unset it.
  -- No output means no changes.
  let
    decoded = Aeson.decode @(Map Text (Maybe Text)) =<< out
    changes = foldMap Map.toList decoded

  traverse_ (uncurry set) changes
  where
    set name = maybe
      (Env.unsetEnv $ posix name)
      $ \value -> Env.setEnv (posix name) (posix value) True
    posix :: Text -> PosixString
    posix = PosixString . toShort . encodeUtf8

-- | A prompt element for direnv status.
direnvPrompt :: IO String
direnvPrompt = do
  status <- runDirenv ["status", "--json"]
  let allowed = Aeson.parseMaybe loadedRC =<< Aeson.decode =<< status
  pure $ if allowed == Just (0 :: Int) then " (direnv)" else ""
  where
    -- Whether the .envrc direnv last loaded here is allowed: 0 if it is, 1 if
    -- blocked (not allowed yet, or changed since), 2 if denied. It's null
    -- outside of any .envrc. direnv sets its DIRENV_ variables for a blocked or
    -- denied .envrc too, so that it notices when you allow it, so they can't
    -- tell.
    loadedRC = Aeson.withObject "status" $ \o -> o .: "state" >>= (.: "loadedRC") >>= (.: "allowed")
