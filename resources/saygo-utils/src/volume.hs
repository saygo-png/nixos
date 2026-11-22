{-# LANGUAGE TemplateHaskell #-}

import Bin
import GHC.Clock (getMonotonicTime)
import Options.Applicative
import Relude
import System.IO (hPutStrLn, hWaitForInput)
import System.Process (proc, readProcessWithExitCode, waitForProcess, withCreateProcess)

main :: IO ()
main = do
  change <- execParser parserInfo
  dir <- maybe (die "volume: XDG_RUNTIME_DIR is not set") pure =<< lookupEnv "XDG_RUNTIME_DIR"
  case change of
    Just name -> writeFileText (dir <> "/volume.sock") (name <> "\n")
    -- One handle to wob for every press, as each new one allocates its buffers.
    Nothing -> withFile (dir <> "/wob.sock") WriteMode $ \wob -> do
      hSetBuffering wob LineBuffering
      -- A muted device shows as an empty bar.
      daemon $ \s -> hPutStrLn wob (show (if s.muted then 0 else s.level))

-- | A device's volume in percent, and whether it's muted.
data Sink = Sink {level :: Int, muted :: Bool}

-- | Applies the changes sent, showing each on wob's bar before wpctl sets it,
-- so the bar keeps up. Exits after 10 seconds without one.
daemon :: (Sink -> IO ()) -> IO ()
daemon display = do
  start <- getMonotonicTime
  next start =<< readDevice sink
  where
    next lastPress current = whenM (hWaitForInput stdin 10000) $ do
      change <- getLine
      now <- getMonotonicTime
      -- Something else may have changed the volume since the last burst of presses.
      s <- if now - lastPress > 1 then readDevice sink else pure current
      case change of
        "mic-mute" -> do
          wpctl ["set-mute", source, "toggle"]
          display =<< readDevice source
          next now s
        "mute" -> do
          display s{muted = not s.muted}
          wpctl ["set-mute", sink, "toggle"]
          next now s{muted = not s.muted}
        _ -> do
          -- Never lowers a volume above 100%, set elsewhere.
          let level = if change == "up" then max s.level (min 100 (s.level + 1)) else max 0 (s.level - 1)
          display s{level}
          wpctl ["set-volume", sink, show level <> "%"]
          next now s{level}
    sink, source :: String
    sink = "@DEFAULT_AUDIO_SINK@"
    source = "@DEFAULT_AUDIO_SOURCE@"

-- | Without pipes for its output, which allocate buffers on every press.
wpctl :: [String] -> IO ()
wpctl args = withCreateProcess (proc wpctlPath args) $ \_ _ _ p -> void (waitForProcess p)

readWpctl :: [String] -> IO String
readWpctl args = (\(_, out, _) -> out) <$> readProcessWithExitCode wpctlPath args ""

wpctlPath :: FilePath
wpctlPath = $(bin "wpctl")

-- | From wpctl's "Volume: 0.40", with " [MUTED]" after it when muted.
readDevice :: String -> IO Sink
readDevice device =
  readWpctl ["get-volume", device] <&> \out -> case words (toText out) of
    _ : volume : rest -> Sink (maybe 0 (round . (* 100)) (readMaybe @Double (toString volume))) ("[MUTED]" `elem` rest)
    _ -> Sink 0 False

-- | A change to send to the daemon, or Nothing to be it.
parserInfo :: ParserInfo (Maybe Text)
parserInfo =
  info
    ( helper
        <*> hsubparser
          ( change "up" "Raise the volume by 1%, up to 100%"
              <> change "down" "Lower the volume by 1%"
              <> change "mute" "Mute or unmute the output"
              <> change "mic-mute" "Mute or unmute the microphone"
              <> command "daemon" (info (pure Nothing) (progDesc "Apply the changes sent, and show the volume with wob"))
          )
    )
    (progDesc "Change the volume and show it with wob, for niri's volume keys")
  where
    change name description = command (toString name) (info (pure (Just name)) (progDesc description))
