module Stream where

import Control.Monad

data Stream m e r v = Pending (m (Stream m e r v))
                    | Data v (Stream m e r v)
                    | End r
                    | Error e

type Chunk e r v = Either (Either e r) v

read :: (Monad m) => (Stream m e r v) -> m (Chunk e r v, Stream m e r v)
read (End r)     = return (Left (Right r), End r)
read (Error e)   = return (Left (Left e), Error e)
read (Data v m)  = return (Right v, m)
read (Pending s) = s >>= \s' -> Stream.read s'

runStream :: (Monad m) => (Stream m e r v) -> m [Chunk e r v]
runStream (End r)     = return [Left (Right r)]
runStream (Error e)   = return [Left (Left e)]
runStream (Data v s)  = runStream s >>= \s' -> return (Right v : s')
runStream (Pending s) = s >>= \s' -> runStream s'

instance (Show v, Show e, Show r) => Show (Stream m e r v) where
  show (End r)     = "->" ++ show r
  show (Error e)   = "!>" ++ show e
  show (Data v s ) = show v ++ show s
  show (Pending _) = "..."

instance (Monad m) => Functor (Stream m e r) where
  fmap f (End r)     = End r
  fmap f (Error e)   = Error e
  fmap f (Data v s)  = Data (f v) (fmap f s)
  fmap f (Pending s) = Pending (s >>= \s' -> return (fmap f s'))
