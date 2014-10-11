module StreamReader where

import Stream

import Control.Monad.State
import System.Random

streamState :: State StdGen (Chunk e String Int)
streamState = do
  gen <- get
  let (i, gen2) = next gen
  put gen2
  return (Value i)

streamIO :: IO (Chunk e String Int)
streamIO = do
  gen <- getStdGen
  let (i, gen2) = next gen
  setStdGen gen2
  return (Value i)


readTest :: (Monad m) => StreamReaderT e r v m [Chunk e r v]
readTest = do
  a <- Stream.read
  b <- Stream.read
  c <- Stream.read
  d <- Stream.read
  return [a, b, c, d]

readTestRun = do
  runStreamReaderT readTest $ (mapV (2*)) (produce streamIO)
