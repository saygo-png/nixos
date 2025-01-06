-- Modules in ~/.xmonad/lib directory
import Defaults
import EventHandling
import Flow
import KeyBindings
import Layout
import LogHook
import XMonad.Hooks.ManageDocks
import ManageHook
import MouseBindings
import StartupHook
import XMonad
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar

-- main :: IO ()
-- main =
--   myConfig
--     |>
--       ewmh
--       .> ewmhFullscreen
--       -- .> toggleFullFloatEwmhFullscreen
--       .> withEasySB (statusBarProp "xmobar ~/.config/xmobar/.xmobarrc" (pure def)) toggleStrutsKey
--       .> docks
--       .> xmonad
--   where
--     toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
--     toggleStrutsKey XConfig{ modMask = m } = (m, xK_t)
--     myConfig =
--       def
--         { terminal = myTerminal,
--           focusFollowsMouse = myFocusFollowsMouse,
--           clickJustFocuses = myClickJustFocuses,
--           borderWidth = myBorderWidth,
--           modMask = myModMask,
--           workspaces = myWorkspaces,
--           normalBorderColor = myNormalBorderColor,
--           focusedBorderColor = myFocusedBorderColor,
--           keys = myKeys,
--           mouseBindings = myMouseBindings,
--           manageHook = myManageHook,
--           layoutHook = myLayoutHook,
--           handleEventHook = myEventHook,
--           logHook = myLogHook,
--           startupHook = myStartupHook
--         }

main :: IO ()
main = myConfig |> xmobar .> ewmh .> ewmhFullscreen .> xmonad
-- main =
--   xmonad
--     . ewmhFullscreen
--     . ewmh
--     . xmobar
--     $ myConfig
  where
    xmobar = withEasySB (statusBarProp "xmobar ~/.config/xmobar/.xmobarrc" (pure def)) toggleStrutsKey
      where
        toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
        toggleStrutsKey XConfig{modMask = m} = (m, xK_t)
    myLayout = lessBorders OnlyScreenFloat $ avoidStruts $ layoutHook def
    myConfig =
      def
        { layoutHook = myLayout,
          keys = myKeys,
          terminal = myTerminal,
          focusFollowsMouse = myFocusFollowsMouse,
          clickJustFocuses = myClickJustFocuses,
          borderWidth = myBorderWidth,
          modMask = myModMask,
          workspaces = myWorkspaces,
          normalBorderColor = myNormalBorderColor,
          focusedBorderColor = myFocusedBorderColor,
          mouseBindings = myMouseBindings,
          manageHook = myManageHook,
          handleEventHook = myEventHook,
          logHook = myLogHook,
          startupHook = myStartupHook
        }
