module Main where

import Config
import Control.Exception (throw)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as BL8
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
import Data.Text (Text)
import Data.Text.Encoding.Error (strictDecode)
import Data.Time (Day, UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (formatParseM, iso8601ParseM, iso8601Show)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Flow
import GHC.IO.Handle.FD (openFile)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import List
import Options.Applicative
import System.Directory (XdgDirectory (XdgData), doesFileExist, getXdgDirectory)
import System.Posix (getFileStatus)
import Types
import Take


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
    CmdTake -> takeDrug
    CmdList -> listDrugs
