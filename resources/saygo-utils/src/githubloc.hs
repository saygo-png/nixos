{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as Csv
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Format (parseTimeM)
import Data.Vector qualified as V
import Graphics.Rendering.Chart.Backend.Diagrams qualified as Chart
import Graphics.Rendering.Chart.Easy hiding (Vector, argument)
import Options.Applicative
import Universum

data Options = Options {repoName :: String, csvFile :: FilePath}

data Entry = Entry
  { time :: UTCTime
  , additions :: Int
  , deletions :: Int
  }
  deriving stock (Show)

data LocEntry = LocEntry
  { time :: UTCTime
  , linesOfCode :: Int
  }
  deriving stock (Show)

instance Csv.FromNamedRecord Entry where
  parseNamedRecord r =
    Entry <$> r Csv..: "DateTime" <*> r Csv..: "Additions" <*> r Csv..: "Deletions"

instance Csv.FromField UTCTime where
  parseField t =
    parseTimeM True defaultTimeLocale "%F" (toString . decodeUtf8 @Text $ t)

instance Csv.ToField UTCTime where
  toField t =
    encodeUtf8 . toText $ formatTime defaultTimeLocale "%F" t

main :: IO ()
main = do
  opts <- execParser parserInfo
  csvData <- stripUtf8Bom <$> BL.readFile opts.csvFile
  case Csv.decodeByName csvData :: Either String (Csv.Header, Vector Entry) of
    Left err -> putStrLn err
    Right (_, vector) -> do
      absoluteLoc <- case toAbsoluteLoc 0 vector of
        Nothing -> die "Csv has no entries!"
        Just loc -> pure loc
      print absoluteLoc

      let price1 =
            def
              { _plot_lines_values = [toList $ map (\e -> (e.time, e.linesOfCode)) absoluteLoc]
              , _plot_lines_title = "Lines of code"
              }
          layout =
            def
              { _layout_title = "Lines of code history for " <> opts.repoName
              , _layout_plots = [toPlot price1]
              }

      _ <- Chart.renderableToFile def "githubLinesOfCodeChart.png" $ toRenderable layout
      pass

-- https://github.com/haskell-hvr/cassava/issues/106
stripUtf8Bom :: BL.ByteString -> BL.ByteString
stripUtf8Bom bs = fromMaybe bs (BL.stripPrefix "\239\187\191" bs)

toAbsoluteLoc :: Int -> Vector Entry -> Maybe (Vector LocEntry)
toAbsoluteLoc linesOfCode vector
  | V.null vector = Nothing
  | otherwise = Just . snd $ go linesOfCode vector
  where
    sumLines currentLoc entry =
      currentLoc + entry.additions + entry.deletions

    go :: Int -> Vector Entry -> (Int, Vector LocEntry)
    go currentLoc entries =
      case V.uncons entries of
        Nothing -> (currentLoc, V.empty)
        Just (entry, rest) ->
          let newLoc = sumLines currentLoc entry
              (finalLoc, restEntries) = go newLoc rest
           in (finalLoc, V.cons (LocEntry entry.time newLoc) restEntries)

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser) (progDesc "Convert Github's \"Code Frequency\" graph to absolute LOC over time")
  where
    optionsParser :: Parser Options
    optionsParser =
      Options
        <$> argument str (metavar "REPO_NAME" <> help "Name of the github repo, purely visual")
        <*> argument str (metavar "CSV_FILE" <> help "Github's \"Code Frequency\" csv file")
