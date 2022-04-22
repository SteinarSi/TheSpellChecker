{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}


module UnsizedMatrix where


import qualified Data.Vector as UV
import qualified Data.Vector.Sized as V
import GHC.TypeLits (KnownNat, SomeNat(..), someNatVal)
import Data.Proxy (Proxy(..))
import Data.Maybe (fromJust)


{-
import Matrix

data UMatrix a = UMatrix { unUMatrix :: UV.Vector (UV.Vector a), height :: Int, width :: Int }


toUnsized :: forall m n a. (KnownNat m, KnownNat n) => Matrix m n a -> UMatrix a
toUnsized = undefined 
--toUnsized a = UMatrix { unUMatrix = fmap (\v -> ) (unMatrix a), height = mS a, width = nS a }


unaryFunc :: forall m n h i a. (Matrix m n a -> Matrix h i a) -> UMatrix a -> UMatrix a
unaryFunc f (UMatrix (V.SomeSized v) _ _) = undefined $ f (Matrix (fmap (\(V.SomeSized u) -> u) v))

--sizing :: forall n a. (KnownNat n) => UV.Vector a -> V.Vector n a
--sizing (V.SomeSized v) = v

fromLists :: [[a]] -> UMatrix a
fromLists = undefined 


withSized :: forall a r. UV.Vector a -> (forall n m. (KnownNat n, KnownNat m) => Matrix m n a -> r) -> r
withSized v f = case someNatVal (fromIntegral (V.length v)) of
  Just (SomeNat (Proxy :: Proxy n)) -> case someNatVal (fromIntegral (V.length $ V.head v)) of
      Just (SomeNat (Proxy :: Proxy m)) -> f (fromJust $ fromList $ map Vector v :: Matrix m n a)
      _ -> undefined 
  Nothing -> error "impossible: Vector has negative length"
-}
{-
lengthV :: forall a b. KnownNat a => V.Vector a b -> Integer
lengthV _ = natVal @a Proxy

testFunc :: Unsized.Vector Integer -> Integer
testFunc (V.SomeSized v) = lengthV v
    --sum (V.zipWith (+) v (V.replicate 1))
        -- ^ here, v is `Sized.Vector n Int`, and we have `KnownNat n`

testV :: Unsized.Vector Int
testV = Unsized.fromList [1, 2, 3]
-}