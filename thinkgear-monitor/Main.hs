import Data.Binary.Machine (streamGet)
import Data.Machine
import Network.Bluetooth.Adapter
import Network.Bluetooth.Device
import System.IO.Machine (sourceIO, printer)
import System.Environment (getArgs)

import Codec.Binary.ThinkGear (getPacket)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BSB16
import qualified Data.ByteString.Char8 as BSC8

-- TODO Contribute to `machines-io`
-- byChunk ::  IOData a => IODataMode a
-- byChunk = (\h -> hGetChunk h, \h xs -> hPut h xs)
-- byChunkOf :: Int -> IODataMode ByteString
-- byChunkOf n = (\h -> BS.hGet h n, \h xs -> BS.hPut h xs)

monitor :: String -> IO ()
monitor btaddr = do
  Just adapter <- defaultAdapter
  socket <- openRFCOMM (Device adapter addr) 1
  runT_ $ printer <~ streamGet getPacket <~ sourceTG socket where
    sourceTG socket = sourceIO $ recvRFCOMM socket bufferSize
    addr = BluetoothAddr . BS.reverse . fst . BSB16.decode . BSC8.pack $ filter p btaddr where
      p = (/=) ':'
    bufferSize = 16

main :: IO ()
main = do
  args <- getArgs
  case args of
    [port] -> monitor port
    _      -> fail "Invalid argument, please pass bluetooth device address (FF:FF:FF:FF:FF:FF)."
