{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as Csv
import Data.Time (Day (ModifiedJulianDay), UTCTime (utctDay), defaultTimeLocale, formatTime, toModifiedJulianDay)
import Data.Time.Format (parseTimeM)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Granite.Svg qualified as Granite
import Options.Applicative
import Relude

data Options = Options {repoName :: String, csvFile :: FilePath, balance :: Bool}

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

data BalanceEntry = BalanceEntry
  { time :: UTCTime
  , balance :: Int
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
  vector <- case Csv.decodeByName csvData :: Either String (Csv.Header, Vector Entry) of
    Left err -> die err
    Right (_, vector) -> pure vector

  case opts of
    Options{balance = True} -> do
      let balanceVec = toBalance vector
          balanceLine = Granite.series "Net change" . toList $ V.map (\e -> (dayValue e.time, fromIntegral e.balance)) balanceVec

      writeFileText "githubBalanceChart.svg"
        $ Granite.lineGraph [balanceLine] (plotConfig $ "Balance history for " <> opts.repoName)
      putTextLn "Balance chart created!"
    _ -> do
      absoluteLoc <- case toAbsoluteLoc 0 vector of
        Nothing -> die "Csv has no entries!"
        Just loc -> pure loc
      let locLine = Granite.series "Lines of code" . toList $ V.map (\e -> (dayValue e.time, fromIntegral e.linesOfCode)) absoluteLoc

      writeFileText "githubLinesOfCodeChart.svg"
        $ Granite.lineGraph [locLine] (plotConfig $ "Lines of code history for " <> opts.repoName)
      putTextLn "Lines of code chart created!"

-- Granite plots Doubles, so dates go on the x axis as Modified Julian Day numbers
dayValue :: UTCTime -> Double
dayValue = fromIntegral . toModifiedJulianDay . utctDay

plotConfig :: String -> Granite.Plot
plotConfig title =
  Granite.defPlot
    { Granite.plotTitle = toText title
    , Granite.widthChars = 70
    , Granite.heightChars = 28
    , Granite.legendPos = Granite.LegendRight
    , Granite.xNumTicks = 6
    , Granite.yNumTicks = 6
    , Granite.xFormatter = \_ _ v -> toText . formatTime defaultTimeLocale "%F" $ ModifiedJulianDay (round v)
    , Granite.yFormatter = \_ _ v -> show (round v :: Int)
    }

-- https://github.com/haskell-hvr/cassava/issues/106
stripUtf8Bom :: BL.ByteString -> BL.ByteString
stripUtf8Bom bs = fromMaybe bs (BL.stripPrefix "\239\187\191" bs)

toBalance :: Vector Entry -> Vector BalanceEntry
toBalance = V.map (\e -> BalanceEntry{time = e.time, balance = e.additions + e.deletions})

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
        <*> switch (long "balance" <> help "Whether to create a balance chart instead")
