-- Modules in ~/.xmonad/lib directory
import Defaults
import EventHandling
import Flow
import KeyBindings
import Layout
import LogHook
import ManageHook
import MouseBindings
import StartupHook
import XMonad
import XMonad.Actions.ToggleFullFloat
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

main :: IO ()
main =
  myConfig
    |>
      ewmh
      .> ewmhFullscreen
      -- .> toggleFullFloatEwmhFullscreen
      .> withEasySB (statusBarProp "xmobar ~/.config/xmobar/.xmobarrc" (pure def)) toggleStrutsKey
      .> docks
      .> xmonad
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_t)
    myConfig =
      def
        { terminal = myTerminal,
          focusFollowsMouse = myFocusFollowsMouse,
          clickJustFocuses = myClickJustFocuses,
          borderWidth = myBorderWidth,
          modMask = myModMask,
          workspaces = myWorkspaces,
          normalBorderColor = myNormalBorderColor,
          focusedBorderColor = myFocusedBorderColor,
          keys = myKeys,
          mouseBindings = myMouseBindings,
          manageHook = myManageHook,
          layoutHook = myLayoutHook,
          handleEventHook = myEventHook,
          logHook = myLogHook,
          startupHook = myStartupHook
        }
