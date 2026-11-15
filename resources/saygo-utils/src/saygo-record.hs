{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- The first run records the screen, with what the default sink plays and
-- virtual_mic mixed together, until a second run stops it. Then it offers the
-- video to drag and drop.

import Bin
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket_, catch, handle, onException)
import Data.Aeson.Micro (Value (..), decode)
import Data.Map.Strict qualified as Map
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Options.Applicative (ParserInfo, execParser, helper, info, progDesc)
import Relude
import Shh
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, removeFile)
import System.FilePath ((</>))
import System.IO qualified as IO
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process (CreateProcess (..), StdStream (CreatePipe, UseHandle), getPid, proc, waitForProcess, withCreateProcess)
import System.Timeout (timeout)

default (ByteString)

wlScreenrec, pwCli :: FilePath
wlScreenrec = $(bin "wl-screenrec")
pwCli = $(bin "pw-cli")

pwLink, pwDump, notifySend, ripdrag :: Cmd
pwLink = exe $(bin "pw-link")
pwDump = exe $(bin "pw-dump")
notifySend = exe $(bin "notify-send")
ripdrag = exe $(bin "ripdrag")

main :: IO ()
main = do
  execParser parserInfo
  pidFile <- (</> "saygo-record.pid") . fromMaybe "/tmp" <$> lookupEnv "XDG_RUNTIME_DIR"
  recorder pidFile >>= \case
    Just pid -> signalProcess sigINT pid
    Nothing -> do
      file <- videoFile
      saved <-
        bracket_ (writeFileText pidFile . show =<< getProcessID) (removeFile pidFile) (withCombinedSource (recordTo file))
          `onException` notify "Recording failed"
      if saved
        then notify "Recording stopped" >> ripdrag file
        else notify "Recording failed" >> exitFailure

parserInfo :: ParserInfo ()
parserInfo =
  info (helper <*> pass)
    $ progDesc "Record the screen with desktop and microphone audio, or stop the recording"

notify :: String -> IO ()
notify message = do
  putStrLn message
  -- The message is printed either way.
  ignoreFailure $ notifySend "saygo-record" message

-- | The saygo-record that's recording, if one is.
recorder :: FilePath -> IO (Maybe ProcessID)
recorder pidFile = handle (\(_ :: IOException) -> pure Nothing) $ do
  pid <- decodeUtf8 <$> readFileBS pidFile
  -- One that crashed left its PID, which another process may have now.
  name <- readFileBS ("/proc" </> pid </> "comm")
  pure $ guard (name == "saygo-record\n") *> readMaybe pid

videoFile :: IO FilePath
videoFile = do
  dir <- (</> "Videos" </> "screencaptures") <$> getHomeDirectory
  createDirectoryIfMissing True dir
  (dir </>) . formatTime defaultTimeLocale "%y.%m.%d-%H:%M:%S.mp4" <$> getZonedTime

-- | Records until saygo-record gets a SIGINT or SIGTERM, then has wl-screenrec
-- finish the video. Whether it saved one.
recordTo :: FilePath -> IO Bool
recordTo file = do
  withCreateProcess (proc wlScreenrec args) $ \_ _ _ process -> do
    getPid process >>= traverse_ (\pid -> for_ [sigINT, sigTERM] $ \signal -> installHandler signal (Catch (stop pid)) Nothing)
    notify "Recording"
    void (waitForProcess process)
  doesFileExist file
  where
    args = ["--bitrate", "3 MB", "--max-fps", "24", "--audio", "--audio-device", combinedRecord, "--filename", file]
    -- It may have stopped already.
    stop pid = signalProcess sigINT pid `catch` \(_ :: IOException) -> pass

-- | Runs with the combined_record source, which mixes what the default sink
-- plays and virtual_mic. It's pw-cli's, so it goes away with it.
withCombinedSource :: IO a -> IO a
withCombinedSource action =
  withFile "/dev/null" WriteMode $ \nullHandle ->
    withCreateProcess (proc pwCli []){std_in = CreatePipe, std_out = UseHandle nullHandle, std_err = UseHandle nullHandle} $ \input _ _ _ -> do
      for_ input $ \commands -> IO.hPutStrLn commands ("create-node adapter " <> source) >> IO.hFlush commands
      timeout 5_000_000 awaitPorts >>= maybe (fail (combinedRecord <> " got no ports")) pure
      defaultSink >>= maybe (notify "No default sink, not recording desktop audio") (`mix` "monitor")
      mix "virtual_mic" "capture"
      action
  where
    source = "{ factory.name=support.null-audio-sink node.name=" <> combinedRecord <> " node.description=\"Combined Record\" media.class=Audio/Source/Virtual audio.position=[ FL FR ] }"
    -- The session manager gives it ports a moment after it's created.
    awaitPorts = do
      ports <- pwLink "--input" |> captureLines
      unless (encodeUtf8 (combinedRecord <> ":input_FR") `elem` ports) $ threadDelay 50_000 >> awaitPorts

combinedRecord :: String
combinedRecord = "combined_record"

-- | Links a node's <port>_FL and <port>_FR into combined_record.
mix :: String -> String -> IO ()
mix node port =
  tryFailure (for_ ["FL", "FR"] $ \channel -> pwLink (node <> ":" <> port <> "_" <> channel) (combinedRecord <> ":input_" <> channel))
    >>= either (const $ notify ("Not recording audio from " <> node)) pure

-- | The default sink's node name, from PipeWire's default metadata.
defaultSink :: IO (Maybe String)
defaultSink = do
  dump <- pwDump |> capture
  pure . viaNonEmpty head $ do
    Just (Array objects) <- [decode dump]
    Object object <- objects
    Just (Array entries) <- [Map.lookup "metadata" object]
    Object entry <- entries
    guard (Map.lookup "key" entry == Just (String "default.audio.sink"))
    Just (Object value) <- [Map.lookup "value" entry]
    Just (String name) <- [Map.lookup "name" value]
    pure (toString name)
