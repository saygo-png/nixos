module KeyBindings where

import qualified Data.Map as M
import Defaults
import Graphics.X11.ExtraTypes.XF86
-- import XMonad.Hooks.StatusBar

import TrueFullscreen
import qualified WindowState as WS
import XMonad
import XMonad.Actions.Search
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Gaps
import qualified XMonad.Layout.Magnifier as Magnifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import qualified XMonad.StackSet as W
-- import XMonad.Util.Paste
import XMonad.Util.Run

-- Key bindings. Add, modify or remove key bindings here.
-- mod1Mask = alt
-- shiftMask = shift
toggleFullscreenWithStruts :: X ()
toggleFullscreenWithStruts = do
  sendMessage $ Toggle NBFULL
  sendMessage ToggleStruts

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig{XMonad.modMask = modm}) =
  M.fromList $
    -- launch a terminal
    [ ((modm, xK_Return), safeSpawn (XMonad.terminal conf) [])
    , ((modm, xK_y), toggleFullscreen)
    , -- launch browser
      ((modm, xK_b), safeSpawn myBrowser [])
    , -- launch rofi and dashboard
      ((modm, xK_space), safeSpawn "rofi" ["-show", "drun"])
    , ((mod1Mask, xK_Tab), safeSpawn "rofi" ["-show", "window"])
    , ((mod1Mask, xK_Tab), safeSpawn "rofi" ["-show", "window"])
    , ((modm, xK_p), safeSpawn "xkb-switch-keyboard" [])
    , ((modm .|. shiftMask, xK_d), safeSpawn "d3-autocast-menu" [])
    , ((modm .|. shiftMask, xK_space), safeSpawn "rofi" ["-show", "run"])
    , ((modm .|. mod1Mask, xK_x), spawn "xkill")
    , -- Toggle floating window
      ((modm .|. shiftMask, xK_f), withFocused WS.toggleFloat)
    , -- Toggle fullscreen layout
      ((modm, xK_f), toggleFullscreenWithStruts)
    , -- Audio keys
      -- ((0, xF86XK_AudioPlay), spawn "playerctl play-pause"),
      -- ((0, xF86XK_AudioPrev), spawn "playerctl previous"),
      -- ((0, xF86XK_AudioNext), spawn "playerctl next"),

      -- -- Brightness keys
      -- ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl s +10%"),
      -- ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl s 10-%"),

      -- Screenshot
      ((0, xK_Print), spawn "flameshot_wrapper")
    , ((modm, xK_Print), spawn "flameshot screen")
    , -- close focused window
      ((modm, xK_q), kill)
    , -- GAPS!!!
      ((modm .|. controlMask, xK_g), sendMessage ToggleGaps) -- toggle all gaps
      -- Rotate through the available layout algorithms
    , ((modm, xK_i), sendMessage NextLayout)
    , --  Reset the layouts on the current workspace to default
      ((modm .|. shiftMask, xK_i), setLayout $ XMonad.layoutHook conf)
    , -- Resize viewed windows to the correct size
      ((modm, xK_n), refresh)
    , -- Move focus to the next window
      ((modm, xK_j), windows W.focusDown)
    , -- Move focus to the previous window
      ((modm, xK_k), windows W.focusUp)
    , -- Move focus to the master window
      ((modm, xK_m), windows W.focusMaster)
    , -- Swap the focused window and the master window
      ((modm, xK_x), windows W.swapMaster)
    , -- Swap the focused window with the next window
      ((modm .|. shiftMask, xK_j), windows W.swapDown)
    , -- Swap the focused window with the previous window
      ((modm .|. shiftMask, xK_k), windows W.swapUp)
    , -- Shrink the master area
      ((modm, xK_h), sendMessage Shrink)
    , -- Expand the master area
      ((modm, xK_l), sendMessage Expand)
    , -- Push window back into tiling
      ((modm, xK_t), withFocused $ windows . W.sink)
    , -- Increment the number of windows in the master area
      ((modm, xK_comma), sendMessage (IncMasterN 1))
    , -- Deincrement the number of windows in the master area
      ((modm, xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. controlMask, xK_equal), sendMessage Magnifier.MagnifyMore)
    , ((modm .|. controlMask, xK_minus), sendMessage Magnifier.MagnifyLess)
    , ((modm .|. controlMask, xK_o), sendMessage Magnifier.ToggleOff)
    , ((modm .|. controlMask .|. shiftMask, xK_o), sendMessage Magnifier.ToggleOn)
    , ((modm .|. controlMask, xK_m), sendMessage Magnifier.Toggle)
    , -- Toggle the status bar gap
      -- Use this binding with avoidStruts from Hooks.ManageDocks.
      -- See also the statusBar function from Hooks.DynamicLog.
      --
      -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

      -- lock pc
      ((modm .|. shiftMask, xK_l), spawn "lock")
    , -- Restart xmonad
      ((modm, xK_r), restart "xmonad" True)
    , ((modm .|. controlMask, xK_r), restart "xmonad" False)
    ]
      ++
      -- Applications --
      [((modm .|. mod1Mask, xK_b), spawn "blueman-manager")]
      ++
      -- Search --
      [ ((modm .|. mod1Mask, xK_h), promptSearch def hoogle)
      , -- , ((modm .|. mod1Mask, xK_n), promptSearch def noogle)
        -- , ((modm .|. mod1Mask, xK_p), promptSearch def nixos)
        ((modm .|. mod1Mask, xK_g), promptSearch def google)
      ]
      ++
      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
      [ ((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++
      --
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      --
      [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]
