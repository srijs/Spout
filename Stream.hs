module Stream where

import Control.Monad

data Stream s e v = Pending s (s -> (Stream s e v))
                  | Data v (Stream s e v)
                  | End
                  | Error e

type Chunk e v = Either (Maybe e) v

read :: (Stream s e v) -> (Chunk e v, Stream s e v)
read (End)         = (Left Nothing, End)
read (Error e)     = (Left (Just e), Error e)
read (Data v m)    = (Right v, m)
read (Pending s p) = Stream.read (p s)

runStream (End)         = []
runStream (Error e)     = [Left e]
runStream (Data v s )   = Right v : runStream s
runStream (Pending s p) = runStream (p s)

instance (Show v, Show e) => Show (Stream s e v) where
  show (End)         = ";"
  show (Error e)     = "!" ++ show e
  show (Data v s )   = show v ++ show s
  show (Pending _ _) = "..."

instance Functor (Stream s e) where
  fmap f (End)         = End
  fmap f (Error e)     = Error e
  fmap f (Data v s)    = Data (f v) (fmap f s)
  fmap f (Pending s p) = Pending s (\s -> fmap f (p s))

instance Monad (Stream s e) where
  return v = Data v End
  (>>=) (End)         _ = End
  (>>=) (Error e)     _ = Error e
  (>>=) (Data v s)    f = mplus (f v) (s >>= f)
  (>>=) (Pending s p) f = Pending s (\s -> (p s) >>= f)

instance MonadPlus (Stream s e) where
  mzero = End
  mplus (End)         s  = s
  mplus (Error e)     _  = Error e
  mplus (Data v s)    s' = Data v (mplus s s')
  mplus (Pending s p) s' = Pending s (\s -> mplus (p s) s')
