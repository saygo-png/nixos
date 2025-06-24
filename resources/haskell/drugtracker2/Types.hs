module Types where

import Data.ByteString.Char8 qualified as BS8
import Data.Csv
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Flow

data FileState = FileNotExists | FileEmpty | FileHasContent

data DrugLine = DrugLine
  { drugData :: Maybe Text,
    dateData :: UTCTime
  }
  deriving (Eq, Show)

instance FromNamedRecord DrugLine where
  parseNamedRecord m =
    DrugLine
      <$> m
        .: "drug"
      <*> m
        .: "date"

instance ToRecord DrugLine where
  toRecord (DrugLine drugData dateData) =
    record [toField drugData, toField dateData]

instance ToNamedRecord DrugLine where
  toNamedRecord (DrugLine drugData dateData) =
    namedRecord
      ["drug" .= drugData, "date" .= dateData]

instance ToField UTCTime where
  toField i = iso8601Show i |> BS8.pack

instance FromField UTCTime where
  parseField i = BS8.unpack i |> iso8601ParseM
