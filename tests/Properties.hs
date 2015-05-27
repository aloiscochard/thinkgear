{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Binary.Get (runGet)
import Data.Binary.Machine
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Machine
import Test.Hspec

import qualified Data.ByteString.Base16 as B16

import Codec.Binary.ThinkGear

decode :: ByteString -> ByteString
decode = fst . B16.decode

main :: IO ()
main = hspec $ do
  describe "Codec.Binary.ThinkGear" $ do
    it "parse a sample packet" $ do
      shouldBe
        (runGet getPacket $ fromStrict $ decode "aaaa080220017e04120560e3")
        [PoorSignal 32, BatteryLevel 126, Attention 18, Meditation 96]
    it "parse a sample packet (machine)" $ do
      shouldBe
       (join . join . runT $ (processGet getPacket) <~ source [decode "aaaa080220017e",decode "04120560e3"])
       [PoorSignal 32, BatteryLevel 126, Attention 18, Meditation 96]
