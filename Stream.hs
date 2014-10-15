{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, UndecidableInstances #-}

module Stream where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.State

data Stream m e r v = Pending (m (Stream m e r v))
                    | Data v (Stream m e r v)
                    | Success r
                    | Failure e

newtype StreamR m v e r = StreamR { unwrapR :: Stream m e r v }
newtype StreamV m e r v = StreamV { unwrapV :: Stream m e r v }

readHead :: (Monad m) => (Stream m e r v) -> m (Stream m e r v)
readHead (Pending p) = p
readHead s           = return s

readChunk :: (Monoid r, Monad m) => (Stream m e r v) -> m (Stream m e r v, Stream m e r v)
readChunk (Success r) = return (Success r, Success r)
readChunk (Failure e) = return (Failure e, Failure e)
readChunk (Data v m)  = return (Data v (Success mempty), m)
readChunk (Pending s) = s >>= readChunk

runStream :: (Monoid r, Monad m) => (Stream m e r v) -> m [Stream m e r v]
runStream (Success r) = return [Success r]
runStream (Failure e) = return [Failure e]
runStream (Data v s)  = liftM (Data v (Success mempty) :) (runStream s)
runStream (Pending s) = s >>= runStream

instance (Eq (m (Stream m e r v)), Eq e, Eq r, Eq v) => Eq (Stream m e r v) where
  (==) (Success r) (Success r') = r == r'
  (==) (Failure e) (Failure e') = e == e'
  (==) (Data v m)  (Data v' m') = v == v' && m == m'
  (==) (Pending p) (Pending p') = p == p'
  (==) _ _ = False

instance (Show v, Show e, Show r) => Show (Stream m e r v) where
  show (Success r) = "=" ++ show r
  show (Failure e) = "!" ++ show e
  show (Data v s)  = show v ++ ">" ++ show s
  show (Pending _) = "..."

{- Monoid -}

instance (Monoid r, Monad m) => Monoid (Stream m e r v) where
  mempty = Success mempty
  mappend (Success r) s = fmapR (mappend r) s
  mappend (Failure e) _ = Failure e
  mappend (Data v s') s = Data v (mappend s' s)
  mappend (Pending p) s = Pending (liftM (\s' -> mappend s' s) p)

{- Result Functor -}

fmapR :: (Monad m) => (a -> b) -> Stream m e a v -> Stream m e b v
fmapR f (Success r) = Success (f r)
fmapR _ (Failure e) = Failure e
fmapR f (Data v s)  = Data v (fmapR f s)
fmapR f (Pending p) = Pending (liftM (fmapR f) p)

instance (Monad m) => Functor (StreamR m v e) where
  fmap f = StreamR . (fmapR f) . unwrapR

{- Result Applicative -}

pureR :: (Monad m) => r -> Stream m e r v
pureR = returnR

apR :: (Monad m) => Stream m e (a -> b) v -> Stream m e a v -> Stream m e b v
apR m a = bindR m (\f -> bindR a (\s -> returnR (f s)))

instance (Monad m) => Applicative (StreamR m v e) where
  pure = StreamR . pureR
  (<*>) (StreamR m) (StreamR a) = StreamR (apR m a)

{- Result Monad -}

returnR :: r -> Stream m e r v
returnR = Success

bindR :: (Monad m) => Stream m e a v -> (a -> Stream m e b v) -> Stream m e b v
bindR (Success r) f = f r
bindR (Failure e) _ = Failure e
bindR (Data v s)  f = Data v (bindR s f)
bindR (Pending p) f = Pending (liftM (\s -> bindR s f) p)

instance (Monad m) => Monad (StreamR m v e) where
  return = StreamR . returnR
  (>>=) s f = StreamR (bindR (unwrapR s) (unwrapR . f))

{- Value Functor -}

fmapV :: (Monad m) => (v -> w) -> Stream m e r v -> Stream m e r w
fmapV _ (Success r) = Success r
fmapV _ (Failure e) = Failure e
fmapV f (Data v s)  = Data (f v) (fmapV f s)
fmapV f (Pending p) = Pending (liftM (fmapV f) p)

instance (Monad m) => Functor (StreamV m e r) where
  fmap f = StreamV . (fmapV f) . unwrapV

{- Value Applicative - associativity requires `r` to form a commutative monoid -}

pureV :: (Monoid r, Monad m) => v -> Stream m e r v
pureV = returnV

apV :: (Monoid r, Monad m) => Stream m e r (a -> b) -> Stream m e r a -> Stream m e r b
apV m a = bindV m (\f -> bindV a (\s -> returnV (f s)))

instance (Monoid r, Monad m) => Applicative (StreamV m e r) where
  pure = StreamV . pureV
  (<*>) (StreamV m) (StreamV a) = StreamV (apV m a)

{- Value Monad - associativity requires `r` to form a commutative monoid -}

returnV :: (Monoid r, Monad m) => v -> Stream m e r v
returnV v = Data v mempty

bindV :: (Monoid r, Monad m) => Stream m e r v -> (v -> Stream m e r w) -> Stream m e r w
bindV (Data v s)  f = mappend (f v) (bindV s f)
bindV (Pending p) f = Pending (liftM (\s -> bindV s f) p)
bindV (Failure e) _ = Failure e
bindV (Success r) _ = Success r

instance (Monoid r, Monad m) => Monad (StreamV m e r) where
  return = StreamV . returnV
  (>>=) s f = StreamV (bindV (unwrapV s) (unwrapV . f))

{- Misc -}

mapMV :: (Monad m) => Stream m e r v -> (v -> m w) -> Stream m e r w
mapMV (Data v s)  f = Pending (liftM (\w -> Data w (mapMV s f)) (f v))
mapMV (Pending p) f = Pending (liftM (\s -> mapMV s f) p)
mapMV (Failure e) _ = Failure e
mapMV (Success r) _ = Success r

producer :: (Functor m) => m (Stream m e r v) -> Stream m e r v -> Stream m e r v
producer p (Failure e) = Failure e
producer p (Success r) = Success r
producer p (Data v _)  = Data v (produce p)

produce :: (Functor m) => m (Stream m e r v) -> Stream m e r v
produce p = Pending (fmap (producer p) p)

newtype StreamReaderT e r v m s = StreamReaderT { getStateT :: StateT (Stream m e r v) m s }
  deriving (Functor, Applicative, Monad, MonadIO)

runStreamReaderT (StreamReaderT s) = evalStateT s

read :: (Monoid r, Monad m) => StreamReaderT e r v m (Stream m e r v)
read = StreamReaderT $ StateT readChunk
