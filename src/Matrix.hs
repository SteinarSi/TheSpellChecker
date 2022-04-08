
{-# LANGUAGE NoStarIsType, DataKinds, TypeApplications, ScopedTypeVariables, TypeOperators, 
    RankNTypes, DeriveTraversable, TypeFamilies #-}


module Matrix where

import GHC.TypeLits (natVal, KnownNat, Nat, Symbol, type (+), type (*), type (-), type (<=?))
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Sized as V
import Data.List.Split (chunksOf)
import Data.Finite (Finite, finite, getFinite)
import Control.Applicative (liftA2)
import Prelude hiding (compose)
import Data.Type.Bool (If)
import Data.Bool (bool)
import Data.List (findIndices, find)
import Data.Maybe (listToMaybe, fromJust, catMaybes)
import Control.Monad (join, (<=<))
import Utility (deleteNth)


type Vector m a = Matrix m 1 a
newtype Matrix (m :: Nat) (n :: Nat) a = Matrix {unMatrix :: V.Vector n (V.Vector m a)} deriving (Foldable, Functor, Traversable, Eq)

instance forall m n a. (KnownNat m, KnownNat n, Show a) => Show (Matrix m n a) where
    show m | nS m == 1 = show (head $ V.toList (unMatrix m))
           | otherwise = concatMap ((\(Matrix v) -> show (V.toList (V.head v)) ++ "\n") . (`row` m)) [0..finite (natVal @m Proxy - 1)]


instance forall m n a. (KnownNat m, KnownNat n) => Applicative (Matrix m n) where
    (<*>) = zipMatrixWith ($)
    pure = repl

instance forall m n a. (KnownNat m, KnownNat n, Num a) => Num (Matrix m n a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    fromInteger = pure . fromInteger
    abs = fmap abs
    signum = fmap signum


(><) :: forall m n h a. (KnownNat m, KnownNat n, KnownNat h, Num a) => Matrix m n a -> Matrix n h a -> Matrix m h a
(><) a b = Matrix (V.generate (\c -> V.generate (\r -> row r a • col c b)))

repl :: forall m n a. (KnownNat m, KnownNat n) => a -> Matrix m n a
repl = Matrix . V.replicate . V.replicate

transpose :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> Matrix n m a
transpose = rows

(!) :: (KnownNat m, KnownNat n) => Matrix m n a -> (Finite m, Finite n) -> a
(!) (Matrix v) (i, j) = V.index (V.index v j) i

scale :: forall m n a. (KnownNat m, KnownNat n, Num a) => a -> Matrix m n a -> Matrix m n a
scale a = fmap (a*)

pivots :: forall m n a. (KnownNat m, KnownNat n, Num a, Eq a) => Matrix m n a -> [a]
pivots a = map (a !) $ pivotIndices a

pivotIndices :: forall m n a. (KnownNat m, KnownNat n, Num a, Eq a) => Matrix m n a -> [(Finite m, Finite n)]
pivotIndices = catMaybes . V.toList . V.imap (\i v -> listToMaybe $ catMaybes $ V.toList $ V.imap (\j x -> bool Nothing (Just (i, j)) (x/=0)) v) . unMatrix . rows

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
scaleCol c a (Matrix u) = Matrix (V.imap (\c' v -> bool v (fmap (a*) v) (c==c')) u)

vector :: forall n a. V.Vector n a -> Vector n a
vector = Matrix . V.singleton

expose :: forall m n a. Vector n a -> V.Vector n a
expose (Matrix v) = V.head v

compose :: Matrix m n a -> Matrix m h a -> Matrix m (n+h) a
compose (Matrix u) (Matrix v) = Matrix (u V.++ v)

mF :: forall m n a. (KnownNat m) => Matrix m n a -> Finite m
mF _ = fromInteger (natVal @m Proxy - 1)

nF :: forall m n a. (KnownNat n) => Matrix m n a -> Finite n
nF _ = fromInteger (natVal @n Proxy - 1)

mS :: forall m n a i. (KnownNat m, Num i) => Matrix m n a -> i
mS _ = fromInteger $ natVal @m Proxy

nS :: forall m n a i. (KnownNat n, Num i) => Matrix m n a -> i
nS _ = fromInteger $ natVal @n Proxy

col :: forall m n a. (KnownNat n) => Finite n -> Matrix m n a -> Vector m a
col n (Matrix v) = Matrix (V.singleton (V.index v n))

cols :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> Matrix m n a
cols = id

colList :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> [Vector m a]
colList = map vector . V.toList . unMatrix

row :: forall m n a. (KnownNat m, KnownNat n) => Finite m -> Matrix m n a -> Vector n a
row m (Matrix v) = Matrix (V.singleton (V.map (`V.index` m) v))

rows :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> Matrix n m a
rows = Matrix . sequenceA . unMatrix

rowList :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> [Vector n a]
rowList = map vector . V.toList . unMatrix . rows

diag :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> [a]
diag a = map (\i -> a ! (finite i, finite i)) [0..min (nS a) (mS a)-1]

unit :: forall m n b i. (KnownNat n, Num b) => Finite n -> Vector n b
unit a = vector (V.generate (bool 0 1 . (==a)))

identity :: forall n a. (KnownNat n, Num a) => Matrix n n a
identity = Matrix (V.generate (expose . unit))

zipMatrixWith :: forall m n a b c. (a -> b -> c) -> Matrix m n a -> Matrix m n b -> Matrix m n c
zipMatrixWith f (Matrix u) (Matrix v) = Matrix (V.zipWith (V.zipWith f) u v)

dot :: forall n a. (KnownNat n, Num a) => Vector n a -> Vector n a -> a
dot = (sum .) . (*)

(•) :: forall n a. (KnownNat n, Num a) => Vector n a -> Vector n a -> a
(•) = dot

findindex :: forall n a. KnownNat n => Finite n -> (a -> Bool) -> Vector n a -> Maybe (Finite n)
findindex u f a = find (>=u) (map (finite . fromIntegral) (findIndices f (V.toList (expose a))))