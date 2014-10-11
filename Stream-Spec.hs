{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

import Test.Hspec
import Test.QuickCheck

import Text.Show.Functions
import Control.Monad.Identity

import Data.Monoid

import Stream

type R = [Float]
type V = Int
type S v r = Stream Identity Int r v
type SR = S V R
type SRF = S V (R -> R)
type FV = V -> V
type FR = R -> R
type MV = V -> SR
type MR = R -> SR

main :: IO ()
main = hspec $ do

  describe "Monoid" $ do

    it "satisfies Identity axiom" $ property $
      \(x :: SR) -> (mappend mempty x == x) && (mappend x mempty == x)

    it "satisfies Associativity axiom" $ property $
      \(x :: SR, y :: SR, z :: SR) -> mappend x (mappend y z) == mappend (mappend x y) z

  describe "Result Functor" $ do

    it "satisfies Identity axiom" $ property $
      \(x :: SR) -> fmapR id x == id x

    it "satisfies Distributive axiom" $ property $
      \(p :: FR, q :: FR, x :: SR) -> fmapR (p . q) x == ((fmapR p) . (fmapR q)) x

  describe "Result Applicative" $ do

    it "satisfies Identity axiom" $ property $
      \(v :: SR) -> pureR id `apR` v == v

    it "satisfies Composition axiom" $ property $
      \(u :: SRF, v :: SRF, w :: SR) -> pureR (.) `apR` u `apR` v `apR` w == u `apR` (v `apR` w)

    it "satisfies Homomorphism axiom" $ property $
      \(f :: FR, x :: R) -> pureR f `apR` pureR x == (pureR (f x) :: SR)

    it "satisfies Interchange axiom" $ property $
      \(u :: SRF, y :: R) -> u `apR` pureR y == pureR ($ y) `apR` u

    it "is consistent with Functor" $ property $
      \(f :: FR, x :: SR) -> fmapR f x == pureR f `apR` x

  describe "Result Monad" $ do

    it "satisfies Left Identity axiom" $ property $
      \(a :: R, f :: MR) -> (bindR (returnR a) f) == f a

    it "satisfies Right Identity axiom" $ property $
      \(m :: SR) -> bindR m returnR == m

    it "satisfies Associativity axiom" $ property $
      \(m :: SR, f :: MR, g :: MR) -> bindR (bindR m f) g == bindR m (\x -> bindR (f x) g)

  describe "Value Functor" $ do

    it "satisfies Identity axiom" $ property $
      \(x :: SR) -> fmapV id x == id x

    it "satisfies Distributive axiom" $ property $
      \(p :: FV, q :: FV, x :: SR) -> fmapV (p . q) x == ((fmapV p) . (fmapV q)) x

  describe "Value Monad" $ do

    it "satisfies Left Identity axiom" $ property $
      \(a :: V, f :: MV) -> (bindV (returnV a) f) == f a

    it "satisfies Right Identity axiom" $ property $
      \(m :: SR) -> bindV m returnV == m

    it "satisfies Associativity axiom" $ property $
      \(m :: SR, f :: MV, g :: MV) -> bindV (bindV m f) g == bindV m (\x -> bindV (f x) g)

instance (Arbitrary e, Arbitrary r, Arbitrary v) => Arbitrary (Stream m e r v) where
  arbitrary = oneof [
    arbitrary >>= \r -> return (Stream.Success r),
    arbitrary >>= \e -> return (Stream.Failure e),
    arbitrary >>= \v -> arbitrary >>= \s -> return (Stream.Data v s)]
