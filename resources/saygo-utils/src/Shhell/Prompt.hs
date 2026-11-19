module Shhell.Prompt (shhellPrompt) where

import Control.Exception (handle)
import Relude
import Shh.Prompt (PromptFn, formatPrompt)
import Shhell.Direnv (direnvPrompt, direnvSync)
import Shhell.Use (useEnvFix, useEnvPrompt)
import System.IO (hPutStrLn)

shhellPrompt :: PromptFn
shhellPrompt modules line = do
  prompt <- handle fallback $ do
    direnvSync
    useEnvFix
    direnv <- direnvPrompt
    useEnv <- useEnvPrompt
    formatPrompt
      (green <> "%u" <> reset <> "@%h " <> green <> "%w" <> uncoloured direnv <> uncoloured useEnv <> icon <> reset)
      modules
      line
  -- In a Neovim terminal, tell shhell.nvim where the prompt starts and ends
  -- (OSC 133) and which directory the shell is in (OSC 7). GHCi runs this in
  -- the directory it started in, so that comes from $PWD, which shh's cd sets.
  lookupEnv "NVIM" >>= \case
    Nothing -> pure prompt
    Just _ -> do
      dir <- concatMap (\c -> if c == '%' then "%25" else [c]) . fromMaybe "" <$> lookupEnv "PWD"
      pure ("\ESC]7;file://" <> dir <> "\a\ESC]133;A\a" <> prompt <> "\ESC]133;B\a")
  where
    green, reset, icon :: String
    green = "\ESC[0;32m"
    reset = "\ESC[0m"
    icon = " \xeda7 "
    -- Shown in the normal colour, in the middle of the green part.
    uncoloured :: String -> String
    uncoloured "" = ""
    uncoloured s = reset <> s <> green

    -- Catches everything, Ctrl-C included, so you can interrupt a slow .envrc.
    fallback (e :: SomeException) = do
      hPutStrLn stderr $ "shhell: " <> displayException e
      pure "ghci> "
