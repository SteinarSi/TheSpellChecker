
{-# LANGUAGE NoStarIsType, DataKinds, TypeApplications, ScopedTypeVariables, TypeOperators, 
    RankNTypes, DeriveTraversable, TypeFamilies #-}


module Matrix where

import GHC.TypeLits (natVal, KnownNat, Nat, Symbol, type (+), type (*), type (-), type (<=?))
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Sized as V
import Data.List.Split (chunksOf)
import Data.Finite (Finite, finite, getFinite)
import Control.Applicative (liftA2)
import Prelude hiding (compose, replicate)
import Data.Type.Bool (If)
import Data.Type.Coercion (trans)
import Data.Bool (bool)
import Data.List (findIndices, find)
import Data.Maybe (listToMaybe, fromJust)
import Control.Monad (join, (<=<))
import Utility (deleteNth)


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

replicate :: forall m n a. (KnownNat m, KnownNat n) => a -> Matrix m n a
replicate = Matrix . V.replicate . V.replicate

transpose :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> Matrix n m a
transpose = rows

zero :: forall m n a. (KnownNat m, KnownNat n, Num a) => Matrix m n a
zero = 0

index :: (KnownNat m, KnownNat n) => Matrix m n a -> (Finite m, Finite n) -> a
index (Matrix v) (i, j) = V.index (V.index v j) i

(!) :: (KnownNat m, KnownNat n) => Matrix m n a -> (Finite m, Finite n) -> a
(!) = index

scale :: forall m n a. (KnownNat m, KnownNat n, Num a) => a -> Matrix m n a -> Matrix m n a
scale a = fmap (a*)

pivots :: forall m n a. (KnownNat m, KnownNat n, Num a) => Matrix m n a -> [(Finite m, Finite n)]
pivots a = undefined 

fromList :: forall m n a. (KnownNat m, KnownNat n) => [[a]] -> Maybe (Matrix m n a)
fromList = fmap (transpose . Matrix) . V.fromList <=< mapM V.fromList

swap :: forall m n a. (KnownNat m, KnownNat n) => Finite m -> Finite m -> Matrix m n a -> Matrix m n a
swap r1 r2 a = transpose (Matrix (V.imap (\i r -> if i == r1 then expose (row r2 a) else if i == r2 then expose (row r1 a) else r) (unMatrix (rows a))))

addRow :: forall m n a. (KnownNat m, KnownNat n, Fractional a) => Finite m -> Finite n -> Matrix m n a -> Matrix m n a
addRow i j a = transpose (Matrix (V.imap (\i' r -> bool (r + V.map (*(- a ! (i', j))) roww) r (i == i')) (unMatrix (rows a))))
    where roww = expose $ row i a


scaleRow :: forall m n a. (KnownNat m, KnownNat n, Num a) => Finite m -> a -> Matrix m n a -> Matrix m n a
scaleRow r a m = transpose (scaleCol r a (transpose m))

scaleCol :: forall m n a. (KnownNat m, KnownNat n, Num a) => Finite n -> a -> Matrix m n a -> Matrix m n a
scaleCol c a (Matrix u) = Matrix (V.imap (\c' v -> if c == c' then fmap (a*) v else v) u)

vector :: forall n a. V.Vector n a -> Vector n a
vector = Matrix . V.singleton

expose :: forall m n a. Vector n a -> V.Vector n a
expose (Matrix v) = V.head v

compose :: Matrix m n a -> Matrix m h a -> Matrix m (n+h) a
compose (Matrix u) (Matrix v) = Matrix (u V.++ v)

col :: forall m n a. (KnownNat n) => Finite n -> Matrix m n a -> Vector m a
col n (Matrix v) = Matrix (V.singleton (V.index v n))

cols :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> Matrix m n a
cols = id

row :: forall m n a. (KnownNat m, KnownNat n) => Finite m -> Matrix m n a -> Vector n a
row m (Matrix v) = Matrix (V.singleton (V.map (`V.index` m) v))

rows :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> Matrix n m a
rows = Matrix . sequenceA . unMatrix

diag :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> [a]
diag a = map (\i -> a ! (finite i, finite i)) [0..min (natVal @m Proxy) (natVal @n Proxy) -1]

unit :: forall m n b i. (KnownNat n, Num b) => Finite n -> Vector n b
unit a = vector (V.generate (\i -> if i == a then 1 else 0))

identity :: forall n a. (KnownNat n, Num a) => Matrix n n a
identity = Matrix (V.generate (expose . unit))

zipMatrixWith :: forall m n a b c. (a -> b -> c) -> Matrix m n a -> Matrix m n b -> Matrix m n c
zipMatrixWith f (Matrix u) (Matrix v) = Matrix (V.zipWith (V.zipWith f) u v)

dot :: forall n a. (KnownNat n, Num a) => Vector n a -> Vector n a -> a
dot = (sum .) . (*)

aij :: forall m n a. (KnownNat m, KnownNat n) => (Finite (m+1), Finite (n+1)) -> Matrix (m+1) (n+1) a -> Matrix m n a
aij (i, j) (Matrix v) = Matrix (fromJust (V.fromList (deleteNth (getFinite j) (map (fromJust . V.fromList . deleteNth (getFinite i) . V.toList) (V.toList v)))))

(•) :: forall n a. (KnownNat n, Num a) => Vector n a -> Vector n a -> a
(•) = dot

findindex :: forall n a. KnownNat n => Finite n -> (a -> Bool) -> Vector n a -> Maybe (Finite n)
findindex u f a = find (>= u) (map (finite . fromIntegral) (findIndices f (V.toList (expose a))))