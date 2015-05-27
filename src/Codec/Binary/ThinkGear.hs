module Codec.Binary.ThinkGear where

import Data.Binary.Get
import Data.Binary.IEEE754 (getFloat32be)
import Data.Binary.Put (Put, putWord16be)
import Data.Bits (complement)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Int
import Data.Maybe (maybeToList)
import Data.Word

import qualified Data.ByteString as BS

-- http://developer.neurosky.com/docs/doku.php?id=thinkgear_communications_protocol

data DataRow =
    BatteryLevel Word8
  | PoorSignal Word8
  | HeartRate Word8
  | Attention Word8
  | Meditation Word8
  | RawWave8 Word8
  | RawWave Int16
  | EEGPower Float Float Float Float Float Float Float Float
  | RPeaksInterval
  deriving (Eq, Show)

getPacket :: Get [DataRow]
getPacket = do
  payload <- getPacket'
  return $ runGet getDataRows (fromStrict payload)

getPacket' :: Get ByteString
getPacket' = do
  syncs   <- findSyncs
  if syncs /= 2 then getPacket' else do
    pLength <- getWord8
    if pLength >= n then getPacket' else do
      payload <- getByteString $ fromIntegral pLength
      chksum  <- getWord8
      if chksum /= (complement . sum $ BS.unpack payload) then fail "invalid checksum" else return payload
  where
    findSyncs :: Get Int
    findSyncs = f 0 where
      f i = lookAhead getWord8 >>= \sync ->
        if sync == n then skip 1 >> f (i + 1)
        else if i < 2 then skip 1 >> f 0
        else return i
    n = 170

getDataRow :: Get (Maybe DataRow)
getDataRow = do
  _     <- getExCodeLvl
  code  <- getWord8
  if code <= 127 then do
    value <- getWord8
    return $ case code of
      1 -> Just $ BatteryLevel value
      2 -> Just $ PoorSignal value
      3 -> Just $ HeartRate value
      4 -> Just $ Attention value
      5 -> Just $ Meditation value
      6 -> Just $ RawWave8 value
      _ -> Nothing
  else do
    len <- getWord8
    case (code, len) of
      (128, 2)  -> Just . RawWave . fromIntegral <$> getWord16be
      (129, 32) -> fmap Just $ EEGPower <$> getFloat32be <*> getFloat32be <*> getFloat32be <*> getFloat32be <*>
                                            getFloat32be <*> getFloat32be <*> getFloat32be <*> getFloat32be
      _         -> skip (fromIntegral len) >> return Nothing
  where
    getExCodeLvl :: Get Int
    getExCodeLvl = f 0 where
      f i = lookAhead getWord8 >>= \x -> if x == n then skip 1 >> f (i + 1) else return i
      n = 85

getDataRows :: Get [DataRow]
getDataRows = f [] where
  f xs = do
    x     <- getDataRow
    empty <- isEmpty
    let ys = (xs ++ maybeToList x)
    if empty then return ys else f ys

data ASIC = ASIC12 | ASIC96 | ASIC57RAW | ASIC57FFT

putASIC :: ASIC -> Put
putASIC ASIC12    = putWord16be 0
putASIC ASIC96    = putWord16be 1
putASIC ASIC57RAW = putWord16be 2
putASIC ASIC57FFT = putWord16be 3
