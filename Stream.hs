module Stream where

import Control.Monad

data Stream s e v = Pending (s -> (Stream s e v)) s
                  | Data v (Stream s e v)
                  | End
                  | Error e

read :: (Stream s e v) -> (Either (Maybe e) v, Stream s e v)
read (End)         = (Left Nothing, End)
read (Error e)     = (Left (Just e), Error e)
read (Data v m)    = (Right v, m)
read (Pending p s) = Stream.read (p s)

runStream (End)         = []
runStream (Error e)     = [Left e]
runStream (Data v s )   = Right v : runStream s
runStream (Pending p s) = runStream (p s)

instance (Show v, Show e) => Show (Stream s e v) where
  show (End)         = ";"
  show (Error e)     = "!" ++ show e
  show (Data v s )   = show v ++ show s
  show (Pending _ _) = "..."

instance Functor (Stream s e) where
  fmap f (End)         = End
  fmap f (Error e)     = Error e
  fmap f (Data v s)    = Data (f v) (fmap f s)
  fmap f (Pending p s) = Pending (\s -> fmap f (p s)) s

instance Monad (Stream s e) where
  return v = Data v End
  (>>=) (End)         _ = End
  (>>=) (Error e)     _ = Error e
  (>>=) (Data v s)    f = mplus (f v) (s >>= f)
  (>>=) (Pending p s) f = Pending (\s -> (p s) >>= f) s

instance MonadPlus (Stream s e) where
  mzero = End
  mplus (End)         s  = s
  mplus (Error e)     _  = Error e
  mplus (Data v s)    s' = Data v (mplus s s')
  mplus (Pending p s) s' = Pending (\s -> mplus (p s) s') s
