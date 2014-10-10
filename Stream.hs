module Stream where

import Control.Monad

data Stream m e r v = Pending (m (Stream m e r v))
                    | Data v (Stream m e r v)
                    | Success r
                    | Failure e

data Chunk e r v = Value v | Result r | Error e
  deriving Show

read :: (Monad m) => (Stream m e r v) -> m (Chunk e r v, Stream m e r v)
read (Success r) = return (Result r, Success r)
read (Failure e) = return (Error e, Failure e)
read (Data v m)  = return (Value v, m)
read (Pending s) = s >>= Stream.read

runStream :: (Monad m) => (Stream m e r v) -> m [Chunk e r v]
runStream (Success r) = return [Result r]
runStream (Failure e) = return [Error e]
runStream (Data v s)  = liftM (Value v :) (runStream s)
runStream (Pending s) = s >>= runStream

instance (Show v, Show e, Show r) => Show (Stream m e r v) where
  show (Success r) = "->" ++ show r
  show (Failure e) = "!>" ++ show e
  show (Data v s ) = show v ++ show s
  show (Pending _) = "..."

instance (Functor m) => Functor (Stream m e r) where
  fmap f (Success r) = Success r
  fmap f (Failure e) = Failure e
  fmap f (Data v s)  = Data (f v) (fmap f s)
  fmap f (Pending s) = Pending (fmap (fmap f) s)
