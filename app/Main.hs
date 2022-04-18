
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts,
   FlexibleInstances, GADTs, KindSignatures, MultiParamTypeClasses,
   NoStarIsType, PolyKinds, ScopedTypeVariables, TypeApplications, TypeFamilies,
   TypeOperators, UndecidableInstances #-}

module Main where


import GHC.TypeLits (natVal, KnownNat, Nat, Symbol, type (+), type (*))

import Data.Text (Text, pack, unpack)
import TextShow (toString, showb)

import Interpreter (loop)

main :: IO ()
main = loop [] []
