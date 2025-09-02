{-# LANGUAGE DeriveAnyClass #-}

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as J
import Data.Aeson.Types qualified as J
import GHC.Generics
import Safe
import Shh
import Universum

data Keyboard = Keyboard {main :: Bool, layout :: String}
  deriving stock (Generic)
  deriving anyclass (J.FromJSON)

main :: IO ()
main = do
  jsonInput <- hyprctl "devices" "-j" |> captureTrim
  options <- getMainLayouts jsonInput & maybe (die "Failed to get main layout") pure
  choice <- writeOutput options |> rofiChoose |> captureTrim <&> decodeUtf8 @String

  hyprctl "switchxkblayout" "current" choice

hyprctl, rofi :: Cmd
hyprctl = exe "hyprctl"
rofi = exe "rofi"

rofiChoose :: Proc ()
rofiChoose = rofi "-dmenu" "-i" "-format" "i" "-sep" "," "-p" "Switch keyboard layout to"

getMainLayouts :: LByteString -> Maybe String
getMainLayouts jsonData = do
  obj <- J.decode jsonData
  keyboards :: [Keyboard] <- J.parseMaybe (J..: J.fromString "keyboards") obj
  headMay . map (.layout) $ filter (.main) keyboards
