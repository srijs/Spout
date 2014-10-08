module StreamReader where

import Stream
import System.Random
import Data.Functor.Identity
import Control.Monad
import Control.Monad.State

produce :: (s -> Chunk e (s, v)) -> s -> Stream s e v
produce p s = case p s of
  (Left Nothing) -> End
  (Left (Just e)) -> Error e
  (Right (s, v)) -> Data v (Pending s (produce p))

stream2 :: (Int, StdGen) -> (Chunk e ((Int, StdGen), Int))
stream2 (3, gen) = (Left Nothing)
stream2 (n, gen) = let (i, gen2) = next gen in Right ((n+1, gen2), i)

type StreamReaderT s e v = StateT (Stream s e v)

read :: (Monad m) => StreamReaderT s e v m (Chunk e v)
read = get >>= (\(out, st) -> put st >> return out) . Stream.read

readTest = do
  a <- StreamReader.read
  b <- StreamReader.read
  c <- StreamReader.read
  d <- StreamReader.read
  return [a, b, c, d]

readTestRun = do
  gen <- newStdGen
  evalStateT readTest (fmap (2*) (produce stream2 (0, gen)))
