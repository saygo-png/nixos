module List (
  listDrugs,
) where

import ClassyPrelude
import Config
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Function ((&))
import Data.Text qualified as T
import Data.Vector qualified as V
import Text.Layout.Table
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
  let few = takeLast 14 nl
  let table = gridString [def, def, def] (toList (fmap V.toList few))
  putStrLn $ T.pack table

niceLines :: Vector DrugLine -> IO (Vector (Vector Text))
niceLines vec = do
  let nums = V.generate (length vec) (\x -> tshow $ x + 1)
  let names = fromMaybe "DRUG NAME MISSING" . drugData <$> vec
  dates <- traverse dateStamp vec
  return $ zipWith3 (\num name date -> fromList [num, name, date]) nums names dates

dateStamp :: DrugLine -> IO Text
dateStamp dl = do
  let date = dateData dl
  let absTime = T.dropEnd 3 $ takeWhile (/= '.') . tshow $ date
  relTime <- T.pack <$> prettyTimeAutoFromNow date
  return $ relTime <> ", " <> absTime
