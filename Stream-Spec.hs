{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Function
import Test.QuickCheck.Instances

import Text.Show.Functions
import Control.Monad.Identity

import Data.Set (Set)

import Data.Monoid

import Stream

type R = Set Float
type V = Int
type S v r = Stream Identity Int r v
type SR = S V R
type SRF = S V FR
type SVF = S FV R
type SVFun = S (Fun V V) R
type FV = V -> V
type FunV = Fun V V
type FR = R -> R
type MV = V -> SR
type MR = R -> SR

funToSVF (Data (Fun _ f) s) = returnV f
funToSVF (Pending p) = Pending $! liftM funToSVF p
funToSVF (Stream.Success r) = Stream.Success r
funToSVF (Stream.Failure e) = Stream.Failure e

main = defaultMain $ testGroup "Properties"

  [ testGroup "Monoid"

    [ QC.testProperty "Identity" $
        \(x :: SR) -> (mappend mempty x == x) && (mappend x mempty == x)
    , QC.testProperty "Associativity" $
        \(x :: SR, y :: SR, z :: SR) -> mappend x (mappend y z) == mappend (mappend x y) z
    ]

  , testGroup "Result Functor"

    [ QC.testProperty "Identity" $
        \(x :: SR) -> fmapR id x == id x
    , QC.testProperty "Distributive" $
        \(p :: FR, q :: FR, x :: SR) -> fmapR (p . q) x == ((fmapR p) . (fmapR q)) x
    ]

  , testGroup "Result Applicative"

    [ QC.testProperty "Identity" $
        \(v :: SR) -> pureR id `apR` v == v
    , QC.testProperty "Composition" $
        \(u :: SRF, v :: SRF, w :: SR) -> pureR (.) `apR` u `apR` v `apR` w == u `apR` (v `apR` w)
    , QC.testProperty "Homomorphism" $
        \(f :: FR, x :: R) -> pureR f `apR` pureR x == (pureR (f x) :: SR)
    , QC.testProperty "Interchange" $
        \(u :: SRF, y :: R) -> u `apR` pureR y == pureR ($ y) `apR` u
    , QC.testProperty "Functor" $
        \(f :: FR, x :: SR) -> fmapR f x == pureR f `apR` x
    ]

  , testGroup "Result Monad"

    [ QC.testProperty "Left Identity" $
        \(a :: R, f :: MR) -> (bindR (returnR a) f) == f a
    , QC.testProperty "Right Identity" $
        \(m :: SR) -> bindR m returnR == m
    , QC.testProperty "Associativity" $
        \(m :: SR, f :: MR, g :: MR) -> bindR (bindR m f) g == bindR m (\x -> bindR (f x) g)
    ]

  , testGroup "Value Functor"

    [ QC.testProperty "Identity" $
        \(x :: SR) -> fmapV id x == id x
    , QC.testProperty "Distributive" $
        \(p :: FV, q :: FV, x :: SR) -> fmapV (p . q) x == ((fmapV p) . (fmapV q)) x
    ]

  , testGroup "Value Applicative"

    [ QC.testProperty "Identity" $
        \(v :: SR) -> pureV id `apV` v == v
    , QC.testProperty "Composition" $
        \(u :: SVFun, v :: SVFun, w :: SR) -> pureV (.) `apV` (funToSVF u) `apV` (funToSVF v) `apV` w == (funToSVF u ) `apV` ((funToSVF v) `apV` w)
    , QC.testProperty "Homomorphism" $
        \(f :: FV, x :: V) -> pureV f `apV` pureV x == (pureV (f x) :: SR)
    , QC.testProperty "Interchange" $
        \(u :: SVF, y :: V) -> u `apV` pureV y == pureV ($ y) `apV` u
    , QC.testProperty "Functor" $
        \(f :: FV, x :: SR) -> fmapV f x == pureV f `apV` x
    ]

  , testGroup "Value Monad"

    [ QC.testProperty "Left Identity" $
        \(a :: V, f :: MV) -> (bindV (returnV a) f) == f a
    , QC.testProperty "Right Identity" $
        \(m :: SR) -> bindV m returnV == m
    , QC.testProperty "Associativity" $
        \(m :: SR, (Fun _ (f :: MV)), (Fun _ (g :: MV))) -> bindV (bindV m f) g == bindV m (\x -> bindV (f x) g)
    ]

  ]

instance (Arbitrary e, Arbitrary r, Arbitrary v) => Arbitrary (Stream Identity e r v) where
  arbitrary = oneof [
    arbitrary >>= \r -> return (Stream.Success r),
    arbitrary >>= \e -> return (Stream.Failure e),
    arbitrary >>= \v -> arbitrary >>= \s -> return (Stream.Data v s)]
  shrink (Data v s) = map (Stream.Data v) (shrink s) ++ shrink s
  shrink (Pending p) = shrink (runIdentity p)
  shrink (Stream.Failure e) = map Stream.Failure (shrink e)
  shrink (Stream.Success r) = map Stream.Success (shrink r)
