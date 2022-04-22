withSized :: forall a r. VU.Vector a -> (forall n m. KnownNat n m => Matrix m n a -> r) -> r
withSized v f = case someNatVal (fromIntegral (V.length v)) of
  Just (SomeNat (Proxy :: Proxy n)) -> case someNatVal (fromIntegral (V.length $ V.head v)) of
      Just (SomeNat (Proxy :: Proxy m)) -> f (fromList $ map Vector v :: Matrix n m a)
      _ -> derp
  Nothing -> error "impossible: Vector has negative length"