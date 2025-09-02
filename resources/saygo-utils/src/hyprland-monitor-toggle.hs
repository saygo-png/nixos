import Data.Aeson qualified as J
import Data.Aeson.Key qualified as J
import Shh
import Universum

config :: MonitorsConfig
config = MonitorsConfig "eDP-1" "HDMI-A-2"

data MonitorsConfig = MonitorsConfig {internal :: String, external :: String}

data ActiveMonitor = External | Internal | BothOrMore | None
  deriving stock (Show)

data Monitor = Monitor {name :: String, disabled :: Bool}
  deriving stock (Show)

instance J.FromJSON Monitor where
  parseJSON = J.withObject "Monitor" $ \obj -> do
    name <- obj J..: J.fromString "name"
    disabled <- obj J..: J.fromString "disabled"
    pure $ Monitor name disabled

newtype Workspace = Workspace {id :: Int}

instance J.FromJSON Workspace where
  parseJSON = J.withObject "Workspace" $ \obj -> do
    id' <- obj J..: J.fromString "id"
    pure $ Workspace id'

main :: IO ()
main = do
  allMonitors <- hyprctl "monitors" "all" "-j" |> capture <&> getAllMonitors
  workspaces <- hyprctl "workspaces" "-j" |> capture <&> getAllWorkspaces
  let enableInternal' = (`enableInternal` map (.id) workspaces)

  case getActiveMonitor allMonitors of
    None -> enableInternal' True
    BothOrMore -> enableInternal' False
    External -> enableInternal' True
    Internal -> enableInternal' False
  where
    getAllMonitors :: LByteString -> [Monitor]
    getAllMonitors = ifNothingDecode . J.decode

    getAllWorkspaces :: LByteString -> [Workspace]
    getAllWorkspaces = ifNothingDecode . J.decode

    ifNothingDecode = fromMaybe (error $ fromString "JSON decoding error")

getActiveMonitor :: [Monitor] -> ActiveMonitor
getActiveMonitor allMonitors
  | monitorsNotFound = error (fromString "Specified monitors don't exist")
  | not internalActive && not externalActive = None
  | internalActive && externalActive = BothOrMore
  | length allMonitors > 1 && externalActive = External
  | length allMonitors > 1 && internalActive = Internal
  | length allMonitors == 1 = error (fromString "Only one monitor is available")
  | otherwise = error (fromString "Unknown error")
  where
    active = filter (not . (.disabled)) allMonitors
    internalActive = any (\x -> x.name == config.internal) active
    externalActive = any (\x -> x.name == config.external) active
    monitorsNotFound =
      not (any (\x -> x.name `elem` [config.internal, config.external]) allMonitors)

enableInternal :: Bool -> [Int] -> IO ()
enableInternal enable ids = do
  let targetOn = bool config.external config.internal enable
      targetOff = bool config.external config.internal (not enable)

  hyprctl "keyword" "monitor" (targetOn <> ",preferred,0x0,1")
  moveWorkspacesToInternal (not enable) ids
  hyprctl "keyword" "monitor" (targetOff <> ", disable")
  where
    moveWorkspacesToInternal :: Bool -> [Int] -> IO ()
    moveWorkspacesToInternal b =
      mapM_ moveWorkspaceToMonitor
      where
        moveWorkspaceToMonitor x =
          hyprctl "dispatch" "moveworkspacetomonitor" (show @String x) target
        target = bool config.external config.internal b

hyprctl :: Cmd
hyprctl = exe "hyprctl"
