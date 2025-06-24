module Take where

import Config
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Csv qualified as Cassava
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (XdgDirectory (XdgData), doesFileExist, getXdgDirectory)
import Types

takeDrug :: IO ()
takeDrug = do
  output <- dataDir
  testDrug <- DrugLine (Just "test") <$> getCurrentTime
  filestate <- getFileState output
  case filestate of
    FileEmpty -> writeWithHeader testDrug output
    FileNotExists -> writeWithHeader testDrug output
    FileHasContent -> appendWithoutHeader testDrug output

getFileState :: FilePath -> IO FileState
getFileState path = do
  exists <- doesFileExist path
  if not exists
    then pure FileNotExists
    else do
      isEmpty <- BS8.null <$> BS8.readFile path
      pure $ if isEmpty then FileEmpty else FileHasContent

writeWithHeader :: DrugLine -> FilePath -> IO ()
writeWithHeader drug output = do
  let dataForWrite = Cassava.encodeByName csvHeader [drug]
  BL8.writeFile output dataForWrite

appendWithoutHeader :: DrugLine -> FilePath -> IO ()
appendWithoutHeader drug output = do
  let dataForWrite = Cassava.encode [drug]
  BL8.appendFile output dataForWrite
