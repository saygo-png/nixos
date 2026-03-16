module Main (main) where

import Data.Text qualified as T
import Relude

main :: IO ()
main = do
  args <- getArgs
  filePath <- case args of
    [f] -> pure f
    _ -> die "Provide a filepath to strip nix paths from"
  file <- decodeUtf8 <$> readFileBS filePath
  putText $ clean file

clean :: Text -> Text
clean = T.concat . go
  where
    go :: Text -> [Text]
    go t =
      case T.breakOn nixStorePath t of
        (front, "") -> pure front
        (front, back) -> front : go (T.drop dropLength back)

nixStorePath :: Text
nixStorePath = "/nix/store/"

dropLength :: Int
dropLength = T.length nixStorePath + hashLength + hyphenAfterHash
  where
    hashLength :: Int = 32
    hyphenAfterHash :: Int = 1
