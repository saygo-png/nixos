module List where
import Data.Csv qualified as Cassava
import Data.ByteString.Lazy.Char8 qualified as BL8

import Types
import Config

listDrugs :: IO ()
listDrugs = do
  fileData <- BL8.readFile =<< dataDir
  let decodedCsv = Cassava.decodeByName @DrugLine fileData
  print decodedCsv
