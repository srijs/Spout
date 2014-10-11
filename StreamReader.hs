module StreamReader where

import Stream

import Data.Monoid
import Control.Monad.State
import System.Random

streamState :: State StdGen (Stream m e String Int)
streamState = do
  gen <- get
  let (i, gen2) = next gen
  put gen2
  return (Data i (Success mempty))

streamIO :: IO (Stream m e String Int)
streamIO = do
  gen <- getStdGen
  let (i, gen2) = next gen
  setStdGen gen2
  return (Data i (Success mempty))


readTest :: (Monoid r, Monad m) => StreamReaderT e r v m [Stream m e r v]
readTest = do
  a <- Stream.read
  b <- Stream.read
  c <- Stream.read
  d <- Stream.read
  return [a, b, c, d]

readTestRun = do
  runStreamReaderT readTest $ (fmapV (2*)) (produce streamIO)
