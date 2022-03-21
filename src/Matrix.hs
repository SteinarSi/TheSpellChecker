
{-# LANGUAGE NoStarIsType, KindSignatures, DataKinds, TypeApplications, ScopedTypeVariables, TypeOperators, RankNTypes, DeriveTraversable #-}

module Matrix where

import GHC.TypeLits (natVal, KnownNat, Nat, Symbol, type (+), type (*), type (-))
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Sized as V
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import Data.Finite
import Control.Applicative (liftA2)
import Prelude hiding (compose, replicate)


type Vector m a = Matrix m 1 a
newtype Matrix (m :: Nat) (n :: Nat) a = Matrix {unMatrix :: V.Vector n (V.Vector m a)} deriving (Foldable, Functor, Traversable, Eq)

instance forall m n a. (KnownNat m, KnownNat n, Show a) => Show (Matrix m n a) where
    show m = concatMap ((\(Matrix v) -> show (V.toList (V.head v)) ++ "\n") . (`row` m)) [0..finite (natVal @m Proxy - 1)]

instance forall m n a. (KnownNat m, KnownNat n) => Applicative (Matrix m n) where
    (<*>) = zipMatrixWith ($)
    pure = replicate

instance forall m n a. (KnownNat m, KnownNat n, Num a) => Num (Matrix m n a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    fromInteger = pure . fromInteger
    abs = fmap abs
    signum = fmap signum


(><) :: forall m n h a. (KnownNat m, KnownNat n, KnownNat h, Num a) => Matrix m n a -> Matrix n h a -> Matrix m h a
(><) a b = Matrix (V.generate (\c -> V.generate (\r -> row r a • col c b)))
    where is = id --V.generate id

replicate :: forall m n a. (KnownNat m, KnownNat n) => a -> Matrix m n a
replicate = Matrix . V.replicate . V.replicate

transpose :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> Matrix n m a
transpose = rows

zero :: forall m n a. (KnownNat m, KnownNat n, Num a) => Matrix m n a
zero = 0

index :: (KnownNat m, KnownNat n) => (Finite m, Finite n) -> Matrix m n a -> a
index (i, j) (Matrix v) = V.index (V.index v j) i

scale :: forall m n a. (KnownNat m, KnownNat n, Num a) => a -> Matrix m n a -> Matrix m n a
scale a = fmap (a*)

vector :: forall n a. V.Vector n a -> Vector n a
vector = Matrix . V.singleton

expose :: forall m n a. Vector n a -> V.Vector n a
expose (Matrix v) = V.head v

compose :: forall m n h a. Matrix m n a -> Matrix m h a -> Matrix m (n+h) a
compose (Matrix u) (Matrix v) = Matrix (u V.++ v)

col :: forall m n a. (KnownNat n) => Finite n -> Matrix m n a -> Vector m a
col n (Matrix v) = Matrix (V.singleton (V.index v n))

cols :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> Matrix m n a
cols = id

row :: forall m n a. (KnownNat m, KnownNat n) => Finite m -> Matrix m n a -> Vector n a
row m (Matrix v) = Matrix (V.singleton (V.map (`V.index` m) v))

rows :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> Matrix n m a
rows = Matrix . sequenceA . unMatrix

unit :: forall m n b i. (KnownNat n, Num b) => Finite n -> Vector n b
unit a = Matrix (V.singleton (V.generate (\i -> if i == a then 1 else 0)))

identity :: forall n a. (KnownNat n, Num a) => Matrix n n a
identity = Matrix (fromJust (V.fromList (map (expose . unit) [0..finite (natVal @n Proxy - 1)])))

zipMatrixWith :: forall m n a b c. (a -> b -> c) -> Matrix m n a -> Matrix m n b -> Matrix m n c
zipMatrixWith f (Matrix u) (Matrix v) = Matrix (V.zipWith (V.zipWith f) u v)

dot :: forall n a. (KnownNat n, Num a) => Vector n a -> Vector n a -> a
dot = (sum .) . (*)

(•) :: forall n a. (KnownNat n, Num a) => Vector n a -> Vector n a -> a
(•) = dot
