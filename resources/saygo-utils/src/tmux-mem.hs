import Data.ByteString.Char8 qualified as C8
import Data.Char (isDigit)
import Text.Printf (printf)
import Universum

main :: IO ()
main = do
  meminfo <- C8.readFile "/proc/meminfo"
  let freeRam = extractNumber . matchLineWith . fromList $ C8.lines meminfo
  putStrLn @Text . fromString $ printf "MemF %.1fG" freeRam

matchLineWith :: Vector ByteString -> ByteString
matchLineWith = fromMaybe C8.empty . find (C8.isPrefixOf target)
  where
    target = C8.pack "MemAvailable:"

extractNumber :: ByteString -> Double
extractNumber = maybe 0 (/ 1048576) . readMaybe . C8.unpack . C8.filter isDigit
