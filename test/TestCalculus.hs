
{-# LANGUAGE DataKinds #-}

module TestCalculus where

import Test.LeanCheck
import Data.Eq.Approximate
import TypeLevel.Number.Nat

import Expr
import Calculus (simplify)
import Utility




import Debug.Trace


type ApproxDouble = AbsolutelyApproximateValue (Digits Z) Double


propSimplify :: Expr ApproxDouble -> Bool
propSimplify ex | b = b
                | otherwise = seq (showErr ex) b
    where b = eval ex == eval (simplify ex)
    
showErr :: (RealFloat n, Show n) => Expr n -> Expr n
showErr ex = trace ("\nExpr: " ++ debug ex ++ "\n" ++ debug (simplify ex) ++ "\n" ++ show (eval ex) ++ " /= " ++ show (eval (simplify ex)) ++ "\n") ex