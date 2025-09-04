{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Vector qualified as V
import Foreign
import Foreign.C
import Numeric (showFFloat)
import Universum

foreign import capi safe "sys/statvfs.h statvfs" statvfs :: CString -> Ptr a -> IO CInt

-- Inlined most of https://hackage.haskell.org/package/byteunits-0.4.0.3 and https://hackage.haskell.org/package/disk-free-space-0.1.0.1

main :: IO ()
main = do
  bytes <- fromIntegral <$> getFreeSpace "/"
  putStrLn . getShortHand . getAppropriateUnits $ ByteValue bytes Bytes

data ByteUnit where
  Bytes :: ByteUnit
  KibiBytes :: ByteUnit
  MebiBytes :: ByteUnit
  GibiBytes :: ByteUnit
  TebiBytes :: ByteUnit
  PebiBytes :: ByteUnit
  ExbiBytes :: ByteUnit

data ByteValue where
  ByteValue :: Float -> ByteUnit -> ByteValue

getFreeSpace :: FilePath -> IO Int
getFreeSpace path =
  withCString path $ \cPath ->
    allocaBytes 112 $ \stat -> do
      throwErrnoPathIfMinus1_ "getDiskUsage" path $ statvfs cPath stat
      frsize <- (`peekByteOff` 8) stat :: IO CULong
      bavail <- (`peekByteOff` 32) stat :: IO Word64
      let frsize' = fromIntegral frsize
      pure $ frsize' * fromIntegral bavail

getBytes :: ByteValue -> Float
getBytes (ByteValue v bu) = case bu of
  Bytes -> v
  KibiBytes -> v * (1024 ** 1)
  MebiBytes -> v * (1024 ** 2)
  GibiBytes -> v * (1024 ** 3)
  TebiBytes -> v * (1024 ** 4)
  PebiBytes -> v * (1024 ** 5)
  ExbiBytes -> v * (1024 ** 6)

convertByteUnit :: ByteValue -> ByteUnit -> ByteValue
convertByteUnit bv bu = case bu of
  Bytes -> ByteValue bytes Bytes
  KibiBytes -> ByteValue (bytes / (1024 ** 1)) KibiBytes
  MebiBytes -> ByteValue (bytes / (1024 ** 2)) MebiBytes
  GibiBytes -> ByteValue (bytes / (1024 ** 3)) GibiBytes
  TebiBytes -> ByteValue (bytes / (1024 ** 4)) TebiBytes
  PebiBytes -> ByteValue (bytes / (1024 ** 5)) PebiBytes
  ExbiBytes -> ByteValue (bytes / (1024 ** 6)) ExbiBytes
  where
    bytes = getBytes bv

getAppropriateUnits :: ByteValue -> ByteValue
getAppropriateUnits bv = fromMaybe bv (V.lastM appropriateUnits)
  where
    bUnits = V.fromList [Bytes, KibiBytes, MebiBytes, GibiBytes, TebiBytes, PebiBytes, ExbiBytes]
    bytes = getBytes bv
    units = map (convertByteUnit (ByteValue bytes Bytes)) bUnits
    appropriateUnits = V.filter (\(ByteValue v _) -> v >= 1.0) units

getShortHand :: ByteValue -> Text
getShortHand (ByteValue v bu) = numText <> buShort
  where
    numText = fromString $ showFFloat (Just 1) v ""
    buShort :: Text = case bu of
      Bytes -> "B"
      KibiBytes -> "Ki"
      MebiBytes -> "Mi"
      GibiBytes -> "Gi"
      TebiBytes -> "Ti"
      PebiBytes -> "Pi"
      ExbiBytes -> "Ei"
