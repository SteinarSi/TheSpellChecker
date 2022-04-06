
{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables, ExplicitNamespaces, NoStarIsType, TypeApplications #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module Linalg where


-- import GHC.TypeLits.KnownNat.Solver
import GHC.TypeLits.Extra
import Matrix
import GHC.TypeLits (natVal, KnownNat, Nat, Symbol, type (+), type (*), type (-))
import Data.Proxy ( Proxy(Proxy) )
import Data.Finite ( Finite, finite, finites, weakenN, getFinite)
import Data.Bool (bool)
import Data.Maybe (fromJust)

import Debug.Trace


reduce :: forall m n a. (KnownNat m, KnownNat n, Fractional a, Eq a) => Matrix m n a -> Matrix m n a
reduce = reduce' 0 0
    where reduce' :: forall m n a. (KnownNat m, KnownNat n, Fractional a, Eq a) => Finite m -> Finite n -> Matrix m n a -> Matrix m n a
          reduce' i j a = case findindex i (/=0) (col j a) of
                            Just i' -> let a'  = swap i i' a
                                           a'' = addRow i j (scaleRow i (1 / a' ! (i, j)) a')
                                       in  bool a'' (reduce' (i+1) (j+1) a'') (getFinite i+1 < natVal @m Proxy && getFinite j+1 < natVal @n Proxy)
                            Nothing -> bool a (reduce' i (j+1) a) (getFinite j+1 < natVal @n Proxy)

echelon :: forall m n a. (KnownNat m, KnownNat n, Fractional a, Eq a) => Matrix m n a -> Matrix m n a
echelon = reduce

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
