import Shh
import Universum

main :: IO ()
main = do
  choice <- writeOutput options |> rofiChoose |> captureTrim
  niri "msg" "action" "switch-layout" choice

niri :: Cmd
niri = exe "niri"

rofi :: Cmd
rofi = exe "rofi"

options :: String
options = "Polish" <> "," <> "Polfinnish"

rofiChoose :: Proc ()
rofiChoose = rofi "-dmenu" "-i" "-format" "i" "-sep" "," "-p" "Switch layout to"
