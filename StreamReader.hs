module StreamReader where

import Stream

import Data.Monoid
import Control.Monad.State
import System.Random

streamState :: Stream (State StdGen) e String Int
streamState = produce $ do
  gen <- get
  let (i, gen2) = next gen
  put gen2
  return (returnV i)

streamIO :: Stream IO e String Int
streamIO = produce $ do
  gen <- getStdGen
  let (i, gen2) = next gen
  setStdGen gen2
  return (returnV i)

readTest :: (Monoid r, Monad m) => StreamReaderT e r v m [Stream m e r v]
readTest = do
  a <- Stream.read
  b <- Stream.read
  c <- Stream.read
  d <- Stream.read
  return [a, b, c, d]

readTest2 :: (Monad m) => Stream m e r Int -> Stream m e r Int
readTest2 s = Pending $ do
  s' <- Stream.hoist s
  case s' of
    (Data i s'') -> return (Data (i * 2) (readTest2 s''))
    (Success r) -> return (Success r)
    (Failure e) -> return (Failure e)

readTestRun = do
  runStreamReaderT readTest $ readTest2 streamIO
