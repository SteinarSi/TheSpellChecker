{-# LANGUAGE FlexibleInstances #-}

module TestCalculus where

import Test.LeanCheck
import Data.Text
import Data.Decimal

import Expr
import Calculus (simplify)



import Debug.Trace


propSimplify :: Expr Double -> Bool
propSimplify ex | b = b
                | otherwise = trace ("\nExpr: " ++ debug ex ++ "\n" ++ show (eval ex) ++ " /= " ++ show (eval (simplify ex))) b
    where b = eval ex ~~ eval (simplify ex)

class AEQ a where
    (~~) :: a -> a -> Bool

instance AEQ a => AEQ (Either Text a) where
    (~~) (Left t1) (Left t2) = t1 == t2
    (~~) (Right x1) (Right x2) = x1 ~~ x2
    (~~) _ _ = False

instance AEQ Double where
    (~~) a b = realFracToDecimal acc a == realFracToDecimal acc b
        where acc = 8