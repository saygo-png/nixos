{-# LANGUAGE QuasiQuotes #-}

-- | direnv integration for the prompt. Both exported functions throw if direnv fails.
-- Guard them when used in the prompt since GHCi exits if its prompt function throws.
module Shhell.Direnv (direnvSync, direnvPrompt) where

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Relude
import Shh (capture, exe, ignoreFailure, (|>))
import System.Directory.OsPath (withCurrentDirectory)
import System.OsPath (osp)
import System.OsString.Internal.Types (OsString (OsString))
import System.OsString.Posix (pstr)
import System.OsString.Posix qualified as PSX
import System.Posix.Env.PosixString qualified as Env
import Text.JSON.Yocto qualified as JSON

-- | Runs direnv with these arguments and decodes the JSON it printed.
-- t`Nothing` if it printed nothing.
runDirenv :: [ByteString] -> IO (Maybe JSON.Value)
runDirenv args = do
  -- GHCi runs the prompt in the directory it started in, not the one you cd'd
  -- to. shh's cd keeps $PWD up to date, so direnv runs there.
  dir <- Env.getEnv [pstr|PWD|] <&> maybe [osp|.|] coerce

  -- direnv exits with an error when, e.g., an .envrc is blocked, but its output
  -- still has to be applied: leaving a loaded directory for a blocked one
  -- unloads the first.
  output <- withCurrentDirectory dir $ exe ("direnv" : args) & ignoreFailure |> capture

  -- It prints nothing when there's nothing to change. yocto's decode calls
  -- error on that, and is lazy, so force it to throw here.
  if LBS.null output
    then pure Nothing
    else Just <$> evaluateWHNF (JSON.decode $ decodeUtf8 output)

-- | Applies the environment changes direnv wants here.
direnvSync :: IO ()
direnvSync = do
  direnvJson <- runDirenv ["export", "json"]

  -- Encode everything before setting anything, so a bad value can't leave the
  -- environment half-updated. A null value means unset.
  changes <- evaluateNF . fold $ do
    JSON.Object m <- direnvJson
    pure $ Map.mapKeys PSX.unsafeEncodeUtf m <&> \case
      JSON.String s -> Just $ PSX.unsafeEncodeUtf s
      _ -> Nothing

  Map.foldMapWithKey set changes
  where
    set name = maybe (Env.unsetEnv name) $ \value -> Env.setEnv name value True

-- | A prompt element showing whether direnv has an .envrc loaded here.
direnvPrompt :: IO String
direnvPrompt = do
  statusJson <- runDirenv ["status", "--json"]

  let
    -- 0 if the loaded .envrc is allowed, 1 if blocked, 2 if denied, and null
    -- outside of any. DIRENV_ variables can't tell: direnv sets them for a
    -- blocked .envrc too, so it notices when you allow it.
    allowed = do
      JSON.Object o <- statusJson
      JSON.Object stateO <- Map.lookup "state" o
      JSON.Object rc <- Map.lookup "loadedRC" stateO
      Map.lookup "allowed" rc

  pure $ if allowed == Just (JSON.Number 0) then " (direnv)" else ""
