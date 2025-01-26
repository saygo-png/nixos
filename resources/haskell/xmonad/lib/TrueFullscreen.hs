module TrueFullscreen where

import Data.List ((\\))
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CLong)
import XMonad
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Prelude (fromMaybe, join, listToMaybe)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Minimize
import XMonad.Util.Types (Direction2D (..))
import XMonad.Util.WindowProperties (getProp32)

-- Toggle fullscreen state for the currently focused window
toggleFullscreen :: X ()
toggleFullscreen = withFocused $ \win -> do
  isFullscreen <- hasProperty "_NET_WM_STATE_FULLSCREEN" win
  setFullscreenState win (not isFullscreen)

-- Set the fullscreen state of a window
setFullscreenState :: Window -> Bool -> X ()
setFullscreenState win isFullscreen = do
  root <- asks theRoot -- Get the root window from the XMonad environment
  withDisplay $ \dpy -> do
    wmState <- getAtom "_NET_WM_STATE"
    fullscreen <- getAtom "_NET_WM_STATE_FULLSCREEN"
    let action = if isFullscreen then 1 else 0 -- 1 to enable, 0 to disable
    io $ allocaXEvent $ \ev -> do
      setEventType ev clientMessage
      -- Populate the client message
      TrueFullscreen.setClientMessageEvent ev win wmState action fullscreen
      -- Send the event to the root window
      sendEvent dpy root False (substructureRedirectMask .|. substructureNotifyMask) ev
    io $ sync dpy False

-- Helper to populate a ClientMessageEvent
setClientMessageEvent :: XEventPtr -> Window -> Atom -> Int -> Atom -> IO ()
setClientMessageEvent ev win wmState action fullscreen = do
  setEventType ev clientMessage
  setClientMessageEvent' ev win wmState 32 [fromIntegral action, fromIntegral fullscreen, 0, 0, 0]

-- Check if a window has a specific property
hasProperty :: String -> Window -> X Bool
hasProperty propName win = do
  withDisplay $ \dpy -> do
    atom <- getAtom propName
    fmap (fromIntegral atom `elem`) $ fromMaybe [] <$> getProp32 atom win
