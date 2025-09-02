module ManageHook where

import Flow
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.StackSet

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: ManageHook
myManageHook =
  composeOne
    [ checkDock -?> doIgnore -- equivalent to manageDocks
    , isDialog -?> doFloat
    , className =? "Gimp" -?> doFloat
    , className =? "MPlayer" -?> doFloat
    , return True -?> doF swapDown
    ]
    <+> composeAll [isFullscreen --> doFullFloat]
