
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts,
   FlexibleInstances, GADTs, KindSignatures, MultiParamTypeClasses,
   NoStarIsType, PolyKinds, ScopedTypeVariables, TypeApplications, TypeFamilies,
   TypeOperators, UndecidableInstances #-}

module Main where


import GHC.TypeLits (natVal, KnownNat, Nat, Symbol, type (+), type (*))

import Data.Text (Text, pack, unpack)
import TextShow (toString, showb)

import Interpreter

main :: IO ()
main = loop []


{-
data Foo (a :: 'Bool) where
    C0 :: Foo 'False
    C1 :: Foo 'True

data Expr a = Const a | IfExpr (Expr Bool) (Expr a) (Expr a)

data Expr a where
    Const :: a -> Expr a
    IfExpr :: Expr Bool -> Expr b -> Expr b -> Expr b
    And :: Expr Bool -> Expr Bool -> Expr Bool
    LessThan :: Expr Int -> Expr Int -> Expr Bool

e1 = Const 5 :: Expr Int
e2 = Const 6 :: Expr Int
e3 = IfExpr (LessThan e1 e2) e1 e2

newtype Matrix (m :: Nat) (n :: Nat) = Matrix [[Double]]

foo :: [[Double]] -> Maybe (Matrix m n)
foo = undefined

hStack :: forall (m :: Nat) (n :: Nat) (o :: Nat). Matrix m n -> Matrix m o -> Matrix m (n + o)
hStack = undefined


-- GHC.TypeLits 
-}