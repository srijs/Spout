module StreamReader where

import Stream
import Control.Monad

stream :: Stream s e [Int]
stream = (Data [1] (Data [2, 3] (Pending (\_ -> Data [4, 5, 6] End) undefined)))

newtype StreamReader s e v a = StreamReader { runStreamReader :: (Stream s e v) -> (a, (Stream s e v)) }

put newStream = StreamReader $ \_ -> ((), newStream)

get = StreamReader (\st -> (st, st))

read = StreamReader (\st -> Stream.read st)

instance Monad (StreamReader s e v) where
  return x = StreamReader (\st -> (x, st))
  (>>=) r f = StreamReader $ \st ->
              let (x, st') = runStreamReader r st
              in runStreamReader (f x) st'

test = do
  a <- StreamReader.read
  b <- StreamReader.read
  c <- StreamReader.read
  return (a, b, c)

testRun = runStreamReader test (fmap (map (2*)) stream)
