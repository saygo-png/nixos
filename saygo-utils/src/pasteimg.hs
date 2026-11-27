{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Bin
import Options.Applicative
import Relude
import Shh

default (ByteString)

wlPaste :: Cmd
wlPaste = exe $(bin "wl-paste")

main :: IO ()
main = do
  file <- execParser parserInfo
  writeFileLBS file =<< (wlPaste "--type" "image/png" |> capture)

parserInfo :: ParserInfo FilePath
parserInfo =
  info
    (helper <*> strArgument (metavar "FILE" <> help "File to save the image to" <> action "file"))
    (progDesc "Save the PNG image in the clipboard to a file")
