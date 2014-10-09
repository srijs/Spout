{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StreamReader where

import Stream
import System.Random
import Control.Applicative
import Control.Monad
import Control.Monad.State

producer :: (Functor m) => m (Chunk e r v) -> Chunk e r v -> Stream m e r v
producer p (Left (Left e))  = Error e
producer p (Left (Right r)) = End r
producer p (Right v)        = Data v (produce p)

produce :: (Functor m) => m (Chunk e r v) -> Stream m e r v
produce p = Pending (fmap (producer p) p)

newtype StreamReaderT e r v m s = StreamReaderT { getStateT :: StateT (Stream m e r v) m s }
  deriving (Functor, Applicative, Monad, MonadIO)

runReaderT (StreamReaderT s) = evalStateT s

read :: (Monad m) => StreamReaderT e r v m (Chunk e r v)
read = StreamReaderT $ StateT Stream.read

{- example -}

streamState :: State StdGen (Chunk e String Int)
streamState = do
  gen <- get
  let (i, gen2) = next gen
  put gen2
  return (Right i)

streamIO :: IO (Chunk e String Int)
streamIO = do
  gen <- getStdGen
  let (i, gen2) = next gen
  setStdGen gen2
  return (Right i)

readTest :: (Monad m) => StreamReaderT e r v m [Chunk e r v]
readTest = do
  a <- StreamReader.read
  b <- StreamReader.read
  c <- StreamReader.read
  d <- StreamReader.read
  return [a, b, c, d]

readTestRun = do
  runReaderT readTest (produce streamIO)
