{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stream where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.State

data Stream m e r v = Pending (m (Stream m e r v))
                    | Data v (Stream m e r v)
                    | Success r
                    | Failure e

data Chunk e r v = Value v | Result r | Error e
  deriving Show

newtype StreamR m v e r = StreamR { unwrapR :: Stream m e r v }

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
  show (Success r) = "=" ++ show r
  show (Failure e) = "!" ++ show e
  show (Data v s)  = show v ++ "," ++ show s
  show (Pending _) = "..."

instance (Functor m) => Functor (Stream m e r) where
  fmap f (Success r) = Success r
  fmap f (Failure e) = Failure e
  fmap f (Data v s)  = Data (f v) (fmap f s)
  fmap f (Pending s) = Pending (fmap (fmap f) s)

{- Result Monoid -}

memptyR :: (Monoid r) => Stream m e r v
memptyR = Success mempty

mappendR :: (Monoid r, Monad m) => Stream m e r v -> Stream m e r v -> Stream m e r v
mappendR (Success r) s = mappendR_ r s
mappendR (Failure e) _ = Failure e
mappendR (Data v s') s = Data v (mappendR s' s)
mappendR (Pending p) s = Pending (liftM (\s' -> mappendR s' s) p)

-- This will be `liftM mappend` at some point?
mappendR_ :: (Monoid r, Monad m) => r -> Stream m e r v -> Stream m e r v
mappendR_ r (Success r') = Success (mappend r r')
mappendR_ _ (Failure e)  = Failure e
mappendR_ r (Data v s)   = Data v (mappendR_ r s)
mappendR_ r (Pending p)  = Pending (liftM (mappendR_ r) p)

instance (Monoid r, Monad m) => Monoid (StreamR m v e r) where
  mempty = StreamR (memptyR)
  mappend (StreamR a) (StreamR b) = StreamR (mappendR a b)

{- Result Functor -}

fmapR :: (Monad m) => (a -> b) -> Stream m e a v -> Stream m e b v
fmapR f (Success r) = Success (f r)
fmapR _ (Failure e) = Failure e
fmapR f (Data v s)  = Data v (fmapR f s)
fmapR f (Pending p) = Pending (liftM (fmapR f) p)

instance (Monad m) => Functor (StreamR m v e) where
  fmap f (StreamR s) = StreamR (fmapR f s)

{- Misc -}

returnV :: (Monoid r) => v -> Stream m e r v
returnV v = Data v (Success mempty)

mapMV :: (Monad m) => Stream m e r v -> (v -> m w) -> Stream m e r w
mapMV (Data v s)  f = Pending (liftM (\w -> Data w (mapMV s f)) (f v))
mapMV (Pending p) f = Pending (liftM (\s -> mapMV s f) p)
mapMV (Failure e) _ = Failure e
mapMV (Success r) _ = Success r

bindV :: (Monad m) => Stream m e r v -> (v -> Stream m e r w) -> Stream m e r w
bindV (Data v s)  f = f v
bindV (Pending p) f = Pending (liftM (\s -> bindV s f) p)
bindV (Failure e) _ = Failure e
bindV (Success r) _ = Success r

mapV :: (Monad m) => (v -> w) -> Stream m e r v -> Stream m e r w
--mapV f s = bindV s (\v -> Data (f v) (mapV f s'))
mapV f = Pending . liftM mapChunk . Stream.readChunk
  where mapChunk (Value v, s') = Data (f v) (mapV f s')
        mapChunk (Error e, _)  = Failure e
        mapChunk (Result r, _) = Success r

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
