import Shh
import Universum

main :: IO ()
main = do
  options <- xkb_switch "--list" |> captureTrim
  choice <- writeOutput options |> rofiChoose |> captureTrim

  xkb_switch "--set" choice
  notify_send $ "Switched layout to" <> show choice

xkb_switch :: Cmd
xkb_switch = exe "xkb-switch"

notify_send :: Cmd
notify_send = exe "notify-send"

rofiChoose :: Proc ()
rofiChoose = rofi "-dmenu" "-i" "-p" "Switch keyboard layout to"
  where
    rofi :: Cmd
    rofi = exe "rofi"
