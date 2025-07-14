module Main where

import ClassyPrelude
import List
import Options.Applicative
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
