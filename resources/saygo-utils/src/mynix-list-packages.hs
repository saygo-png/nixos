{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- Lists the names of the store paths in the current system's closure.

import Data.Text qualified as T
import Relude
import Shh

default (ByteString)

main :: IO ()
main = do
  paths <- exe "nix-store" "--query" "--requisites" "/run/current-system" |> captureLines
  mapM_ putTextLn . sortNub $ map (withoutHash . decodeUtf8) paths
  where
    -- /nix/store/<hash>-<name> to <name>.
    withoutHash = T.drop 1 . T.dropWhile (/= '-')
