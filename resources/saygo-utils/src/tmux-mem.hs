{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Text.Printf (printf)
import Prelude

main :: IO ()
main = printf "MemF %.1fG\n" . ramGB =<< C8.readFile "/proc/meminfo"

ramGB :: ByteString -> Double
ramGB = (/ 1048576) . fromIntegral . parseKB . getLineByMarker "MemAvailable:"

getLineByMarker :: ByteString -> ByteString -> ByteString
getLineByMarker m = C8.drop (C8.length m) . snd . C8.breakSubstring m

parseKB :: ByteString -> Word64
parseKB = fst . fromJust . C8.readWord64 . C8.dropWhile isSpace
