module Shhell.Prompt (shhellPrompt) where

import Control.Exception (handle)
import Relude
import Shh.Prompt (PromptFn, formatPrompt)
import Shhell.Direnv (direnvPrompt, direnvSync)
import System.IO (hPutStrLn)

shhellPrompt :: PromptFn
shhellPrompt modules line = handle fallback $ do
  direnvSync
  direnv <- direnvPrompt
  formatPrompt
    (green <> "%u" <> reset <> "@%h " <> green <> "%w" <> uncoloured direnv <> icon <> reset)
    modules
    line
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
