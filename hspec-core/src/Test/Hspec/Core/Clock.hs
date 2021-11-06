{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Hspec.Core.Clock (
  Seconds(..)
, toMilliseconds
, toMicroseconds
, getTime
, measure
, sleep
, timeout
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Text.Printf
import           Data.Time.Clock.POSIX
import           Control.Concurrent
import qualified System.Timeout as System

newtype Seconds = Seconds Double
  deriving (Eq, Show, Ord, Num, Fractional, PrintfArg)

toMilliseconds :: Seconds -> Int
toMilliseconds (Seconds s) = floor (s * 1000)

toMicroseconds :: Seconds -> Int
toMicroseconds (Seconds s) = floor (s * 1000000)

getTime :: IO Seconds
getTime = do
  t <- getPOSIXTime
  return $ Seconds (realToFrac t)

measure :: IO a -> IO (Seconds, a)
measure action = do
  t0 <- getTime
  a <- action
  t1 <- getTime
  return (t1 - t0, a)

sleep :: Seconds -> IO ()
sleep = threadDelay . toMicroseconds

timeout :: Seconds -> IO a -> IO (Maybe a)
timeout = System.timeout . toMicroseconds
