{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Distribution.Simple.Utils qualified as U
import Distribution.Verbosity (normal)
import GI.Notify qualified
import Options.Applicative
import Shh
import System.Directory (pathIsSymbolicLink)
import System.Directory qualified as D
import System.FilePath
import System.Posix.Files
import Text.Printf (printf)
import Universum

newtype Options = Options {audio :: Bool}

-- pkgs.writeShellApplication {
--       name = "hyprcorder.sh";
--       bashOptions = ["pipefail"];
--       runtimeInputs = with pkgs; [wl-screenrec slurp ripdrag libnotify coreutils procps];
--       text = ''
--         notify() {
--           notify-send "$@"
--           echo "$@"
--         }
--         dir="${conHome}/Videos/screencaptures"
--         [ -d "$dir" ] || mkdir -p "$dir"
--         filename="$dir/$(date +%y.%m.%d-%H:%M).mp4"
--
--         if pgrep wl-screenrec &>/dev/null; then
--           kill -s SIGINT $(pgrep wl-screenrec) && notify "wl-screenrec stopped"
--           pushd "$dir" || exit 2
--           ripdrag "$(find . -maxdepth 1 -type f -printf '%T@\t%Tc %6k Ki\t%P\n' | sort -n | cut -f 3- | tail -n1)"
--           popd || exit 2
--           exit 0
--         fi
--
--         if [ $# -eq 0 ]; then
--           wl-screenrec -b "3 MB" -m 24 -f "$filename" --audio &
--         else
--           dim="$(slurp)"
--           if [ -z "$dim" ]; then
--             notify "No area selected"
--             exit 2
--           fi
--           wl-screenrec -b "3 MB" -m 24 -f "$filename" -g "$dim" &
--         fi
--
--         if pgrep wl-screenrec &>/dev/null; then
--           notify "wl-screenrec started"
--         else
--           notify "wl-screenrec failed to start"
--         fi
--       '';
--     }

main :: IO ()
main = do
  _ <- GI.Notify.init $ Just "saygo-record"
  saygoRecord =<< execParser parserInfo

notify :: Text -> IO ()
notify message = do
  putStrLn message
  GI.Notify.notificationShow =<< GI.Notify.notificationNew "saygo-record" (Just message) Nothing

wlScreenrec, ripdrag :: Cmd
wlScreenrec = exe "wl-screenrec"
ripdrag = exe "ripdrag"

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser) (progDesc "Record the screen")
  where
    optionsParser :: Parser Options
    optionsParser =
      Options
        <$> switch (long "audio" <> short 'a' <> help "Record with mic audio")

saygoRecord :: Options -> IO ()
saygoRecord opts = do
  let audioFlag = bool [] ["--audio"] opts.audio
  notify "Starting recording"
  wlScreenrec $ audioFlag <> ["--bitrate", "3 MB", "--max-fps", "24"]
