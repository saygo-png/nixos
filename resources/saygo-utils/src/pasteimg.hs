{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- Saves the PNG image in the clipboard to a file.

import Bin
import Relude
import Shh

default (ByteString)

wlPaste :: Cmd
wlPaste = exe $(bin "wl-paste")

main :: IO ()
main =
  getArgs >>= \case
    [file] -> writeFileLBS file =<< (wlPaste "--type" "image/png" |> capture)
    _ -> die "Usage: pasteimg <file>"
