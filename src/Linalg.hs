{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables, NoStarIsType, TypeApplications,
             KindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

module Linalg where

import GHC.TypeLits.Extra
import Matrix
import Utility


import GHC.TypeLits (natVal, KnownNat, Nat, Symbol, type (+), type (*), type (-))
import Data.Proxy ( Proxy(Proxy) )
import Data.Finite ( Finite, finite, finites, weakenN, getFinite, weaken, strengthen)
import Data.Bool (bool)
import Data.Maybe (fromJust)
import Control.Monad.Writer (MonadWriter(tell, writer), runWriter, Writer)
import Data.List (foldl', genericLength, genericReplicate)
import Data.Function ((&))
import qualified Data.Vector.Sized as V


import Debug.Trace

newtype VectorSpace n a = Span [Vector n a] deriving Show
data Solution (n :: Nat) a = Unique (Vector n a) | Unlimited (Vector n a) (VectorSpace n a) | Inconsistent
    deriving Show

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
solve a y | elem (nF reduced) (map snd ps) = Inconsistent
          | rank a == nS a = Unique s 
          | otherwise = Unlimited s (Span counter)
          
    where 
        reduced :: (KnownNat m, KnownNat n, Fractional a, Eq a) => Matrix m (n+1) a
        reduced = reduce (compose a y)

        counter :: (KnownNat n, Fractional a, Eq a) => [Vector n a]
        counter  = for freeVars (\j -> 
            unit j + vector (fromJust (V.fromList ([ -reduced ! (finite i, weaken j) | i<-[0..min (nS a) (mS a) -1]] ++ genericReplicate (abs (nS a - mS a)) 0))))

        freeVars :: [Finite n]
        freeVars = filter (`notElem` map (fromJust . strengthen . snd) ps) [0..nF a]
        ps = pivotIndices reduced

        s :: Vector n a
        s = let attempt  = ( [ reduced ! (finite i, nF reduced) | i <- [0..min (nS a) (mS a)-1]])
                attempt2 = attempt ++ replicate (nS a - length attempt) 0
            in  vector $ fromJust $ V.fromList attempt2

det :: forall m n a. (KnownNat m, KnownNat n, Fractional a, Eq a) => Matrix m n a -> a
det a | natVal @n Proxy /= natVal @m Proxy = 0
      | otherwise = let (a', fs) = runWriter (reduceWriter a)
                    in  foldl' (&) (product (diag a')) fs

rank :: forall m n a i. (KnownNat m, KnownNat n, Fractional a, Eq a, Num i) => Matrix m n a -> i
rank = genericLength . pivotIndices . reduce


leastSquares :: forall m n a. (KnownNat m, KnownNat n, Fractional a, Eq a) => Matrix m n a -> Vector m a -> Vector n a
leastSquares a y = s
    where Unique s = solve (transpose a >< a) (transpose a >< y)

nulSpace :: forall m n a. (KnownNat m, KnownNat n, Fractional a, Eq a) => Matrix m n a -> VectorSpace n a
nulSpace a = case solve a 0 of
    Unique _ -> Span []
    Unlimited _ p -> p
    _ -> error "bruh"

--rowSpace :: forall m n a. (KnownNat m, KnownNat n, Fractional a, Eq a) => Matrix m n a -> VectorSpace m a
--rowSpace 


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



s :: Matrix 4 5 Double
s = compose g 0

h :: Matrix 2 3 Double
h = fromJust $ fromList [
        [1, 0, 2],
        [0, 1, 3]
    ]

hy :: Vector 2 Double
hy = vector (fromJust $ V.fromList [5, 6])

i :: Matrix 3 2 Double
i = fromJust $ fromList [
        [1, 5],
        [0, 0],
        [0, 0]
    ]

iy :: Vector 3 Double
iy = vector (fromJust $ V.fromList [10, 0, 0])

j :: Matrix 1 3 Double
j = fromJust $ fromList [[1..3]]

jy :: Vector 1 Double
jy = vector (fromJust $ V.fromList [4])

la :: Matrix 5 3 Double
la = fromJust $ fromList [
        [1, 1, 1],
        [4, 2, 1],
        [9, 3, 1],
        [16, 4, 1],
        [25, 5, 1]
    ]

lay :: Vector 5 Double
lay = vector $ fromJust $ V.fromList [1, 3, 4.5, 7, 10]