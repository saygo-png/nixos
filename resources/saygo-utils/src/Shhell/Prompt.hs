module Shhell.Prompt (shhellPrompt) where

import Relude
import Shh.Prompt (PromptFn, formatPrompt)
import Shhell.Direnv (direnvPrompt, direnvSync)

shhellPrompt :: PromptFn
shhellPrompt modules line = do
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
