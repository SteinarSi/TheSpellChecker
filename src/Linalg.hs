
{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables, ExplicitNamespaces, NoStarIsType, TypeApplications,
             KindSignatures #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module Linalg where

import GHC.TypeLits.Extra
import Matrix
import GHC.TypeLits (natVal, KnownNat, Nat, Symbol, type (+), type (*), type (-))
import Data.Proxy ( Proxy(Proxy) )
import Data.Finite ( Finite, finite, finites, weakenN, getFinite)
import Data.Bool (bool)
import Data.Maybe (fromJust)
import Control.Monad.Writer (MonadWriter(tell, writer), runWriter, Writer)
import Data.List (foldl')
import Data.Function ((&))


data Solution (n :: Nat) a = Unique (Vector n a) | Infinite (Vector n a) [Vector n a] | Inconsistent

reduce :: (KnownNat m, KnownNat n, Fractional a, Eq a) => Matrix m n a -> Matrix m n a
reduce a = let (a', w) = runWriter (reduceWriter a)
            in  a'

reduceWriter :: forall m n a. (KnownNat m, KnownNat n, Fractional a, Eq a) => Matrix m n a -> Writer [a -> a] (Matrix m n a)
reduceWriter = reduceWriter' 0 0
    where reduceWriter' :: forall m n a. (KnownNat m, KnownNat n, Fractional a, Eq a) => Finite m -> Finite n -> Matrix m n a -> Writer [a -> a] (Matrix m n a)
          reduceWriter' i j a = case findindex i (/=0) (col j a) of
                            Nothing -> bool (pure a) (reduceWriter' i (j+1) a) (getFinite j+1 < natVal @n Proxy)
                            Just i' -> do
                                a' <- writer (swap i i' a, bool [] [negate] (i==i'))
                                let scalar = a' ! (i, j)
                                tell [ (* scalar)]
                                let a'' = addRow i j (scaleRow i (1 / scalar) a')
                                bool (pure a'') (reduceWriter' (i+1) (j+1) a'') (getFinite i+1 < natVal @m Proxy && getFinite j+1 < natVal @n Proxy)
                            

solve :: forall m n a. (KnownNat m, KnownNat n, Fractional a, Eq a) => Matrix m n a -> Vector m a -> Solution n a
solve a y = Inconsistent -- TODO
    where s = reduce (compose a y)

det :: forall m n a. (KnownNat m, KnownNat n, Fractional a, Eq a) => Matrix m n a -> a
det a | natVal @n Proxy /= natVal @m Proxy = 0
      | otherwise = let (a', fs) = runWriter (reduceWriter a)
                    in  foldl' (&) (product (diag a')) fs

leastSquares :: forall m n a. (KnownNat m, KnownNat n, Fractional a, Eq a) => Matrix m n a -> Vector m a -> Vector n a
leastSquares a y = s
    where Unique s = solve (transpose a >< a) (transpose a >< y)

a :: Matrix 3 3 Double
a = scaleCol 0 2 (scaleRow 2 3 5)

b = scaleRow 0 0 a
c = scaleCol 0 0 a

d :: Matrix 3 3 Double
d = fromJust $ fromList [
        [1, 4, 8],
        [6, 7, 0],
        [10, -1, 3]
    ]

f :: Matrix 3 4 Double
f = fromJust $ fromList [
        [0, 1, 10, 5],
        [1, 5, 7, 1],
        [6, 7, 4, 3]
    ]

g :: Matrix 4 4 Double
g = fromJust $ fromList [
        [5, -7, 2, 2],
        [0, 3, 0, -4],
        [-5, -8, 0, 3],
        [0, 5, 0, -6]
    ]