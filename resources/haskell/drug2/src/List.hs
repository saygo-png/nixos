module List (
  listDrugs,
) where

import ClassyPrelude
import Config
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Function ((&))
import Data.Text qualified as T
import Data.Vector (generate)
import Text.Time.Pretty
import Types

listDrugs :: IO ()
listDrugs = do
  fileData <- BL8.readFile =<< dataDir
  let decodedCsv = Cassava.decodeByName @DrugLine fileData
  case decodedCsv of
    Left _ -> putStrLn "Error reading database"
    Right (_, vec) ->
      if not (null vec)
        then prettyPrint vec
        else putStrLn "No entries to show!"

takeLast :: (IsSequence seq) => Index seq -> seq -> seq
takeLast i l = reverse l & take i & reverse

prettyPrint :: Vector DrugLine -> IO ()
prettyPrint vec = do
  nl <- niceLines vec
  mapM_ putStrLn $ takeLast 14 nl

niceLines :: Vector DrugLine -> IO (Vector Text)
niceLines vec = do
  let nums = generate (length vec) (\x -> tshow $ x + 1)
  let names = fromMaybe "DRUG NAME MISSING" . drugData <$> vec
  dates <- traverse dateStamp vec
  return $ zipWith3 (\a b c -> addSeps $ fromList [a, b, c]) nums names dates

addSeps :: Vector Text -> Text
addSeps = intercalate " | "

dateStamp :: DrugLine -> IO Text
dateStamp dl = do
  let date = dateData dl
  let absTime = T.dropEnd 3 $ takeWhile (/= '.') . tshow $ date
  relTime <- T.pack <$> prettyTimeAutoFromNow date
  return $ relTime <> ", " <> absTime
