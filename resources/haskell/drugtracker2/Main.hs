--- Imports {{{

-- cassava

import Control.Exception (throw)
import Data.Csv (
  DefaultOrdered (headerOrder),
  FromField (parseField),
  FromNamedRecord (parseNamedRecord),
  Header,
  ToField (toField),
  ToNamedRecord (toNamedRecord),
  (.:),
  (.=),
 )
import Data.Csv qualified as Cassava
import Data.Text.Encoding.Error (strictDecode)
import Flow
import Options.Applicative
import RIO
import RIO.ByteString.Lazy (readFile)
import RIO.Directory (XdgDirectory (XdgData), getXdgDirectory)
import RIO.Text (unpack)
import RIO.Time (Day, defaultTimeLocale, parseTimeM)
import Prelude (print, putStrLn)

-- }}}

-- Config {{{

-- Path relative to $XDG_DATA_HOME
dataDir :: IO FilePath
dataDir = getXdgDirectory XdgData "drug2/data.csv"

-- }}}

-- Parsing {{{
data Command = CmdList | CmdTake

newtype Options = Options
  { optCommand :: Command
  }

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> parseOptions) (progDesc "Reminds you to take a drug")

parseCommand :: Parser Command
parseCommand =
  subparser
    ( command "take" (info (pure CmdTake) (progDesc "Take drug"))
        <> command "list" (info (pure CmdList) (progDesc "List previously taken drug"))
    )

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

main :: IO ()
main = do
  options <- execParser parserInfo
  case optCommand options of
    CmdTake -> putStrLn "taking"
    CmdList -> listDrugs

-- }}}

-- Data types {{{
data DrugLine = DrugLine
  { drugData :: Maybe Text,
    dateData :: Day
  }
  deriving (Eq, Show)

instance FromNamedRecord DrugLine where
  parseNamedRecord m =
    DrugLine
      <$> m
      .: "drug"
      <*> m
      .: "date"

instance FromField Day where
  parseField = decodeUtf8With strictDecode .> unpack .> parseTimeM True defaultTimeLocale "%Y/%m/%d"

-- }}}

takeDrug :: IO ()
takeDrug = undefined

listDrugs :: IO ()
listDrugs = do
  fileData <- readFile =<< dataDir
  let decodedCsv = Cassava.decodeByName @DrugLine fileData
  print decodedCsv
