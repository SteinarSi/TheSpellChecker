
{-# LANGUAGE DataKinds #-}

module TestCalculus where


import Expr (Expr, eval)
import Calculus (simplify)


propSimplify :: Expr Double -> Bool
propSimplify ex = eval ex == eval (simplify ex)
    