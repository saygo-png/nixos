-- Modules in ~/.xmonad/lib directory
{-# LANGUAGE FlexibleContexts #-}

import Defaults
import Flow
-- import LogHook

import XMonad.Util.EZConfig
import KeyBindings
import Layout
import ManageHook
import MouseBindings
import XMonad
import StartupHook
-- import EventHandling

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar

main :: IO ()
main = myConfig |> xmobar .> ewmh .> ewmhFullscreen .> (\xconfig -> getDirectories >>= launch xconfig)
  where
    xmobar = withEasySB (statusBarProp "xmobar ~/.config/xmobar/.xmobarrc" (pure def)) toggleStrutsKey
      where
        toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
        toggleStrutsKey XConfig{modMask = m} = (m, xK_t)
    myConfig =
      def
        { layoutHook = myLayoutHook,
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
          -- handleEventHook = myEventHook,
          -- logHook = myLogHook,
          startupHook = myStartupHook
        }
        `additionalKeysP` [
          ("M--", spawn "wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%-"),
          ("M-=", spawn "wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+")
        ]
