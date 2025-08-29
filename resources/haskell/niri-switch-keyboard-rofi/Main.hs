{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}

import Shh

load SearchPath ["niri", "rofi"]

options :: String
options = "Polish" <> "," <> "Polfinnish"

rofiChoose :: Proc ()
rofiChoose = rofi "-dmenu" "-i" "-format" "i" "-sep" "," "-p" "Switch layout to"

main :: IO ()
main = do
  choice <- writeOutput options |> rofiChoose |> captureTrim
  niri "msg" "action" "switch-layout" choice
