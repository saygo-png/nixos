module Main where

import ClassyPrelude
import List
import Options.Applicative
import Take

data Command = CmdList | CmdTake Text

newtype Options = Options
  { optCommand :: Command
  }

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> parseOptions) (progDesc "Reminds you to take a drug")

parseCommand :: Parser Command
parseCommand =
  subparser
    ( command "take" (info parseTake (progDesc "Take drug"))
        <> command "list" (info (pure CmdList) (progDesc "List previously taken drug"))
    )

parseTake :: Parser Command
parseTake = CmdTake <$> argument str (metavar "DRUG_NAME" <> help "Name of the drug to take")

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

main :: IO ()
main = do
  options <- execParser parserInfo
  case optCommand options of
    CmdTake drugName -> takeDrug drugName
    CmdList -> listDrugs
