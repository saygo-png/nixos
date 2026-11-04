{-# LANGUAGE QuasiQuotes #-}

module Shhell.Direnv (direnvSync) where

import Control.Exception (IOException, try)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Relude
import Shh (capture, exe, ignoreFailure, (|>))
import System.Directory.OsPath (withCurrentDirectory)
import System.OsPath (osp)
import System.OsString.Internal.Types (OsString (OsString), PosixString (PosixString))
import System.OsString.Posix (pstr)
import System.Posix.Env.PosixString qualified as Env

direnvSync :: IO ()
direnvSync = do
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
  -- if the directory is gone, nothing changes.
  out <- try @IOException . withCurrentDirectory dir $ ignoreFailure (exe ["direnv", "export", "json" :: ByteString]) |> capture

  -- Changes to apply reported from direnv. A string to set a variable or null to unset it.
  -- No output means no changes.
  let
    decoded = Aeson.decode @(Map Text (Maybe Text)) =<< rightToMaybe out
    changes = foldMap Map.toList decoded

  traverse_ (uncurry set) changes
  where
    set name = maybe
      (Env.unsetEnv $ posix name)
      $ \value -> Env.setEnv (posix name) (posix value) True
    posix :: Text -> PosixString
    posix = PosixString . toShort . encodeUtf8
