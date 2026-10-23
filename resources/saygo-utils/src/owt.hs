import Control.Exception
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.Csv (encode)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Relude
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath (takeDirectory, (</>))

csvFilePath :: IO FilePath
csvFilePath = do
  docsDir <- getHomeDirectory
  let fileDir = docsDir </> "Documents/owdata.csv"
  print fileDir
  pure fileDir

-- Get the current timestamp in Python's isoformat() style
getTimestamp :: IO String
getTimestamp = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let localTime = utcToLocalTime tz now
  pure $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%6q" localTime

appendToCsv :: String -> IO ()
appendToCsv value = do
  filePath <- csvFilePath
  timestamp <- getTimestamp
  let csvRow = encode [(timestamp, value)]

  createDirectoryIfMissing True (takeDirectory filePath)

  catch
    (BL.appendFile filePath csvRow)
    (\e -> do putStrLn $ "Error writing to file: " ++ show (e :: SomeException))

getUserInput :: String -> IO String
getUserInput prompt = do
  putStr prompt
  hFlush stdout
  map toLower . filter (/= ' ') . toString <$> getLine

main :: IO ()
main = do
  userInput <- getUserInput "Enter 'a' or 'g' (or 'q' to quit): "
  case userInput of
    "a" -> do
      appendToCsv "ams"
      putTextLn $ fromString "Appended 'ams' to the CSV file."
    "g" -> do
      appendToCsv "gen"
      putTextLn $ fromString "Appended 'gen' to the CSV file."
    "q" -> do
      putTextLn $ fromString "Quitting the program."
    _ -> do
      putTextLn $ fromString "Invalid input. Please enter 'a', 'g', or 'q'."
