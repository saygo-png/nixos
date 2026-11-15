{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Data.Text qualified as T
import Options.Applicative
import Relude
import Shh

default (ByteString)

main :: IO ()
main = do
  execParser parserInfo
  paths <- exe "nix-store" "--query" "--requisites" "/run/current-system" |> captureLines
  mapM_ putTextLn . sortNub $ map (withoutHash . decodeUtf8) paths
  where
    -- /nix/store/<hash>-<name> to <name>.
    withoutHash = T.drop 1 . T.dropWhile (/= '-')

parserInfo :: ParserInfo ()
parserInfo = info (helper <*> pass) (progDesc "List the names of the packages in the current system's closure")
