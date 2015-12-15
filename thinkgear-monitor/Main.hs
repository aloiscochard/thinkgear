{-# LANGUAGE Rank2Types #-}
import Control.Applicative
import Data.Binary.Machine (streamGet)
import Data.Binary.Put (runPut)
import Data.Complex
import Data.Machine
import Data.Vector (Vector)
import Network.Bluetooth.Adapter
import Network.Bluetooth.Device
import Numeric.FFT.Vector.Unnormalized (dft)
import System.IO.Machine (sourceIO, printer)
import System.Environment (getArgs)
import Text.Printf (printf)
import Codec.Binary.ThinkGear (ASIC(..), DataRow(..), getPacket, putASIC)

import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base16 as BSB16
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Vector as V
import qualified Numeric.FFT.Vector.Unnormalized as FFT

-- http://www.dspguide.com/ch12/1.htm
-- https://cran.r-project.org/web/packages/seewave/vignettes/seewave_analysis.pdf
-- https://www.clear.rice.edu/elec631/Projects99/mit/index2.htm

-- TODO Contribute to `machines-io`
-- byChunk ::  IOData a => IODataMode a
-- byChunk = (\h -> hGetChunk h, \h xs -> hPut h xs)
-- byChunkOf :: Int -> IODataMode ByteString
-- byChunkOf n = (\h -> BS.hGet h n, \h xs -> BS.hPut h xs)

-- TODO Contribute to `machines-async/stm`
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class
import System.IO.Machine (SinkIO, SourceIO, sinkIO)

import System.Clock

sourceTBMQueue :: TBMQueue a -> SourceIO m a
sourceTBMQueue q = repeatedly $ do
  a <- liftIO . atomically $ readTBMQueue q
  maybe stop yield a

sinkTBMQueue :: TBMQueue a -> SinkIO m a
sinkTBMQueue q = repeatedly $ do
  closed <- liftIO . atomically $ isClosedTBMQueue q
  if closed then stop else do
    a <- await
    liftIO . atomically $ writeTBMQueue q a

asyncQueue :: MonadIO m => forall k. Int -> MachineT IO k a -> ResourceT IO (MachineT m k a)
asyncQueue size producer = do
  q  <- liftIO $ newTBMQueueIO size
  _  <- allocate (acquire q) (release  q)
  return $ sourceTBMQueue q where
    acquire q = async . runT_ $ sinkTBMQueue q <~ producer
    release q a = cancel a >> (atomically $ closeTBMQueue q)

spectro :: Integral a => [a] -> String
spectro xs = concat $ L.intersperse " " zs' where
  zs'  = fmap ((printf "%02d") :: Int -> String) $ take (div (length ys) 2) zs
  zs = V.toList $ fmap round $ fmap ((*) 10) $ ys
  ys = fft_db $ FFT.run dft $ V.fromList $ fmap (\x -> (hamming size $ fromIntegral x) :+ 0) xs
  size = length xs

hanning :: Int -> Double -> Double
hanning size n = 0.5 - 0.5 * cos(2 * pi * n / fromIntegral size)

hamming :: Int -> Double -> Double
hamming size n = 0.54 - 0.46 * cos(2 * pi * n / fromIntegral size)

-- fft_db :: (RealFloat b, Integral a, Ix a) => Array a (Complex b) -> Array a b
fft_db :: (Functor f, RealFloat b) => f (Complex b) -> f b
fft_db = fmap (magsq . logBase 10)

magsq :: RealFloat a => Complex a -> a
magsq (x:+y) = x*x + y*y

sliding :: Int -> Process a [a]
sliding = repeatedly . go [] where
  go [] 0  = stop
  go acc 0 = do
    let xs = reverse acc
    yield xs
    go (reverse $ tail xs) 1
  go acc n = do
    i <- await <|> stop
    go (i:acc) $! n-1

getTimerIO :: MonadIO m => IO (ProcessT m k (TimeSpec, k))
getTimerIO = do
  t0 <- getTime Monotonic
  return $ construct $ f t0
    where
      f t0 = do
        a <- await
        t1 <- liftIO $ getTime Monotonic
        yield (diffTimeSpec t1 t0, a)
        f t1

monitor :: String -> IO ()
monitor btaddr = do
  Just adapter <- defaultAdapter
  socket <- openRFCOMM (Device adapter addr) 1
  -- sendAllRFCOMM socket $ BSL.toStrict $ runPut $ putASIC ASIC57RAW
  runResourceT $ do
    consumer  <- asyncQueue 128 $ asParts <~ streamGet getPacket <~ sourceTG socket
    --timer     <- liftIO $ getTimerIO
    liftIO . runT_ $ printer <~ {-- timer <~ --} auto spectro <~  pipeline <~ consumer
  where
    sourceTG socket = sourceIO $ recvRFCOMM socket bufferSize
    addr = BluetoothAddr . BS.reverse . fst . BSB16.decode . BSC8.pack $ filter p btaddr where
      p = (/=) ':'
    bufferSize = 16
    pipeline = sliding 64 <~ asParts <~ auto f where
      f (RawWave i) = [i]
      f _           = []

main :: IO ()
main = do
  args <- getArgs
  case args of
    [port] -> monitor port
    _      -> fail "Invalid argument, please pass bluetooth device address (FF:FF:FF:FF:FF:FF)."
