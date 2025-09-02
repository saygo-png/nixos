import Data.ByteString.Char8 qualified as BS8
import Data.Time
import Data.Time.Zones
import Data.Time.Zones.All
import Options.Applicative
import Universum

timezones :: [TZLabel]
timezones =
  [ Europe__Warsaw
  , America__New_York
  , Australia__Sydney
  ]

parserInfo :: ParserInfo ()
parserInfo =
  info (helper <*> pass)
    $ progDesc "List times in multiple timezones"

main :: IO ()
main = do
  let timezoneData = zip (map tzByLabel timezones) (map toTZName timezones)
  _ <- execParser parserInfo
  now <- getCurrentTime
  mapM_ (printTime now) timezoneData

printTime :: UTCTime -> (TZ, BS8.ByteString) -> IO ()
printTime utcTime (tz, name) = do
  let local = utcToLocalTimeTZ tz utcTime
      nameString = BS8.unpack name
      timeString = formatTime defaultTimeLocale "%H:%M %F" local
  putStrLn $ timeString ++ " — " ++ nameString
