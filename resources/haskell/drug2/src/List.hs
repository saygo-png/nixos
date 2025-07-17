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

type Row = Vector Text

type Table = Vector Row

padRToLen :: Int -> Text -> Text
padRToLen maxLen t = t <> replicate (maxLen - length t) ' '

prettyTable :: Table -> Text
prettyTable t =
  let cols = V.maximum $ length <$> t

      columnWidths = V.generate cols $
        \col -> V.maximum $ V.mapMaybe (\row -> length <$> (row V.!? col)) t

      formatRow row =
        intercalate " | " $
          V.imap (\col cell -> padRToLen (columnWidths V.! col) cell) row
  in  intercalate "\n" $ formatRow <$> t

prettyPrint :: Vector DrugLine -> IO ()
prettyPrint vec = do
  nl <- niceLines vec
  putStrLn . prettyTable $ takeLast 14 nl

niceLines :: Vector DrugLine -> IO Table
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
