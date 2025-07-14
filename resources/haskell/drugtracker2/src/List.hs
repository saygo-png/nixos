module List where

import ClassyPrelude
import Config
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Function ((&))
import Data.Text qualified as T
import Text.Time.Pretty
import Types

listDrugs :: IO ()
listDrugs = do
  fileData <- BL8.readFile =<< dataDir
  let decodedCsv = Cassava.decodeByName @DrugLine fileData
  case decodedCsv of
    Left _ -> putStrLn "Error reading database"
    Right (_, vec) -> prettyPrint vec

takeLast :: Int -> [a] -> [a]
takeLast i l = reverse l & take i & reverse

prettyPrint :: Vector DrugLine -> IO ()
prettyPrint vec = do
  nl <- niceLines vec
  mapM_ putStrLn $ takeLast 14 nl

niceLines :: Vector DrugLine -> IO [Text]
niceLines vec = do
  dateLines <- traverse prDrugLine vec
  let nums = map ((<> " | ") . tshow) [1 :: Int ..]
  return $ zipWith T.append nums (toList dateLines)

prDrugLine :: DrugLine -> IO Text
prDrugLine dl = do
  t <- T.pack <$> prettyTimeAutoFromNow (dateData dl)
  let nt = T.dropEnd 3 $ takeWhile (/= '.') . tshow $ dateData dl
  return $ t <> ", " <> nt
