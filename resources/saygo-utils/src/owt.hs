{-# LANGUAGE QuasiQuotes #-}

import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.Csv (encode)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import GHC.IO.Handle (hFlush)
import Path (Abs, File, Path, fromAbsFile, parent, relfile, (</>))
import Path.IO (ensureDir, getHomeDir)
import Universum

csvFilePath :: IO (Path Abs File)
csvFilePath = do
  docsDir <- getHomeDir
  let fileDir = docsDir </> [relfile|Documents/owdata.csv|]
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

  ensureDir (parent filePath)

  catch
    (BL.appendFile (fromAbsFile filePath) csvRow)
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
      putStrLn "Appended 'ams' to the CSV file."
    "g" -> do
      appendToCsv "gen"
      putStrLn "Appended 'gen' to the CSV file."
    "q" -> do
      putStrLn "Quitting the program."
    _ -> do
      putStrLn "Invalid input. Please enter 'a', 'g', or 'q'."
