
{-# LANGUAGE ExplicitNamespaces, NoStarIsType, KindSignatures, DataKinds, TypeApplications, ScopedTypeVariables, TypeOperators,
    RankNTypes #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Matrix where

import GHC.TypeLits (natVal, KnownNat, Nat, Symbol, type (+), type (*), type (-))
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Sized as V
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import Data.Finite
import Prelude hiding (compose)


type Vector m a = Matrix m 1 a
newtype Matrix (m :: Nat) (n :: Nat) a = Matrix (V.Vector n (V.Vector m a))

instance forall m n a. (KnownNat m, KnownNat n, Show a) => Show (Matrix m n a) where
    show m = concatMap ((\(Matrix v) -> show (V.toList (V.head v)) ++ "\n") . (`row` m)) [1..natVal @m Proxy]

instance forall m n a. (KnownNat m, KnownNat n, Num a) => Num (Matrix m n a) where
    (+) = zipMatrixWith (+)
    (-) = zipMatrixWith (-)
    (*) = undefined -- TODOOOO
    fromInteger a = Matrix (V.replicate (V.replicate (fromInteger a)))
    abs = fmap abs
    signum = error "This makes no sense for a matrix"

instance forall m n a. Functor (Matrix m n) where
    fmap f (Matrix v) = Matrix (fmap (fmap f) v)

--transpose :: Matrix m n a -> Matrix n m a
--transpose = id -- TODO

zero :: forall m n a. (KnownNat m, KnownNat n, Num a) => Matrix m n a
zero = Matrix (V.replicate (V.replicate 0))

identity :: forall n a. (KnownNat n, Num a) => Matrix n n a
identity = undefined

compose :: forall m n h a. Matrix m n a -> Matrix m h a -> Matrix m (n+h) a
compose (Matrix u) (Matrix v) = Matrix (u V.++ v)

col :: forall m n a. KnownNat n => Integer -> Matrix m n a -> Vector m a
col n (Matrix v) = Matrix (V.singleton (V.index v (finite (n-1))))

row :: forall m n a. (KnownNat m, KnownNat n) => Integer -> Matrix m n a -> Vector n a
row m (Matrix v) = Matrix (V.singleton (V.map (`V.index` finite (m-1)) v))

zipMatrixWith :: forall m n a b c. (a -> b -> c) -> Matrix m n a -> Matrix m n b -> Matrix m n c
zipMatrixWith f (Matrix u) (Matrix v) = Matrix (V.zipWith (V.zipWith f) u v)
