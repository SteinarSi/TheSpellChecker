
{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables, ExplicitNamespaces, NoStarIsType, TypeApplications #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module Linalg where


-- import GHC.TypeLits.KnownNat.Solver
import GHC.TypeLits.Extra
import Matrix
import GHC.TypeLits (natVal, KnownNat, Nat, Symbol, type (+), type (*), type (-))
import Data.Proxy ( Proxy(Proxy) )
import Data.Finite ( Finite, finite, finites, weakenN)
import Data.List (foldl')

reduce :: forall m n a. (KnownNat m, KnownNat n, Fractional a) => Matrix m n a -> Matrix m n a
reduce = reducedEchelon 0 . echelon
    where echelon :: Matrix m n a -> Matrix m n a
          --echelon a = foldl' (\a i -> scaleRow (weakenN i) (1 / a ! (weakenN i, weakenN i)) a) a  (finites :: [Finite (Min n m)])  --([0..] :: [Finite (Min m n)])    --[0..natVal @(Min m n) Proxy]
          echelon a = foldl' (\a i -> addRow i (scaleRow (finite i) (1 / a ! (finite i, finite i)) a)) a [0..min (natVal @m Proxy) (natVal @n Proxy) - 1]
          reducedEchelon _ = id -- TODO
          
a :: Matrix 3 3 Double
a = scaleCol 0 2 (scaleRow 2 3 5)