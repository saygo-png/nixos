module Take (
  takeDrug,
) where

import ClassyPrelude
import Config
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Function
import System.Directory (doesFileExist)
import Text.Printf (printf)
import Types

takeDrug :: Text -> IO ()
takeDrug drugName = do
  output <- dataDir
  drug <- DrugLine (Just drugName) <$> getCurrentTime
  filestate <- getFileState output
  case filestate of
    FileEmpty -> writeWithHeader drug output
    FileNotExists -> writeWithHeader drug output
    FileHasContent -> appendWithoutHeader drug output

getFileState :: FilePath -> IO FileState
getFileState path = do
  exists <- doesFileExist path
  if not exists
    then pure FileNotExists
    else do
      isEmpty <- BS8.null <$> BS8.readFile path
      pure $ if isEmpty then FileEmpty else FileHasContent

wroteInfo :: DrugLine -> IO ()
wroteInfo i = do
  let x = drugData i
  let date = dateData i & tshow & takeWhile (/= '.')
  case x of
    Just name -> printf "Took %s on %s\n" name date
    Nothing -> error "drugData returned Nothing when it shouldn't have"

writeWithHeader :: DrugLine -> FilePath -> IO ()
writeWithHeader drug output = do
  let dataForWrite = Cassava.encodeByName csvHeader [drug]
  BL8.writeFile output dataForWrite
  wroteInfo drug

appendWithoutHeader :: DrugLine -> FilePath -> IO ()
appendWithoutHeader drug output = do
  let dataForWrite = Cassava.encode [drug]
  BL8.appendFile output dataForWrite
  wroteInfo drug
