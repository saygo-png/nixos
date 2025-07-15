module Config (dataDir, csvHeader) where

import ClassyPrelude
import Data.Csv
import Data.Vector qualified as Vector
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)

-- Path relative to $XDG_DATA_HOME
dataDir :: IO FilePath
dataDir = getXdgDirectory XdgData "drug2/data.csv"

csvHeader :: Header
csvHeader =
  Vector.fromList
    [ "drug",
      "date"
    ]
