{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stream where

import Control.Applicative
import Control.Monad
import Control.Monad.State

data Stream m e r v = Pending (m (Stream m e r v))
                    | Data v (Stream m e r v)
                    | Success r
                    | Failure e

data Chunk e r v = Value v | Result r | Error e
  deriving Show

readChunk :: (Monad m) => (Stream m e r v) -> m (Chunk e r v, Stream m e r v)
readChunk (Success r) = return (Result r, Success r)
readChunk (Failure e) = return (Error e, Failure e)
readChunk (Data v m)  = return (Value v, m)
readChunk (Pending s) = s >>= readChunk

runStream :: (Monad m) => (Stream m e r v) -> m [Chunk e r v]
runStream (Success r) = return [Result r]
runStream (Failure e) = return [Error e]
runStream (Data v s)  = liftM (Value v :) (runStream s)
runStream (Pending s) = s >>= runStream

instance (Show v, Show e, Show r) => Show (Stream m e r v) where
  show (Success r) = "->" ++ show r
  show (Failure e) = "!>" ++ show e
  show (Data v s)  = show v ++ "," ++ show s
  show (Pending _) = "..."

instance (Functor m) => Functor (Stream m e r) where
  fmap f (Success r) = Success r
  fmap f (Failure e) = Failure e
  fmap f (Data v s)  = Data (f v) (fmap f s)
  fmap f (Pending s) = Pending (fmap (fmap f) s)


producer :: (Functor m) => m (Chunk e r v) -> Chunk e r v -> Stream m e r v
producer p (Error e)  = Failure e
producer p (Result r) = Success r
producer p (Value v)  = Data v (produce p)

produce :: (Functor m) => m (Chunk e r v) -> Stream m e r v
produce p = Pending (fmap (producer p) p)

newtype StreamReaderT e r v m s = StreamReaderT { getStateT :: StateT (Stream m e r v) m s }
  deriving (Functor, Applicative, Monad, MonadIO)

runStreamReaderT (StreamReaderT s) = evalStateT s

read :: (Monad m) => StreamReaderT e r v m (Chunk e r v)
read = StreamReaderT $ StateT readChunk
