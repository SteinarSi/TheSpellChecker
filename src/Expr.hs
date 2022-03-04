--https://www.mdpi.com/2073-8994/10/7/285
--A Step-by-Step Solution Methodology for Mathematical Expressions 

{-# LANGUAGE OverloadedStrings #-}

module Expr (Expr(..), Argument, Parameter, Function(Function), isConstant, evalConstant, evalFunction, betaReduce) where

import Data.Number.RealCyclotomic (RealCyclotomic, toReal, toRat)
import Data.Text (Text, unpack, pack)
import Data.List (intercalate)

import TextShow (TextShow(..), showbParen, showb, fromText, toText, toString)
import Utility (realToRat)

-- 2 + 3 * 4^5 = Add (Num 2) (Mult (Num 3) (Expo (Num 4) (Num 5)))

data Expr = Add Expr Expr
          | BSub Expr Expr --binær minus
          | USub Expr --unær minus
          | Mult Expr Expr
          | Div Expr Expr
          | Log Expr Expr -- logf(g), der både f og g er funksjoner gitt av noen parametre
          | Expo Expr Expr
           -- | UFunc UnaryFunction Expr
          -- | BFunc BinaryFunctio Expr Expr
          | Num RealCyclotomic
          | Var Text
    deriving (Show, Eq)

type Argument = (Text, RealCyclotomic)
type Parameter = Text

--data UnaryFunction = Sin | Cos | Tan | Sqrt
--data BinaryFunctin = Log

data Function = Function Text [Parameter] Expr

instance Show Function where
    show (Function name params expr) = unpack name <> "(" <> intercalate "," (map unpack params) <> ")=" <> toString (showb expr)

instance TextShow Expr where
    showbPrec p (Num  a)    = showb a
    showbPrec p (Var v)     = fromText v
    showbPrec p (Add  a b)  = showbParen (5 < p) (showbPrec 5 a <> "+" <> showbPrec 5 b)
    showbPrec p (USub a)    = showbParen (5 < p) ("-" <> showbPrec p a)
    showbPrec p (BSub a b) = showbParen (5 < p) (showbPrec 5 a <> "-" <> showbPrec 5 b)
    showbPrec p (Mult a b)  = showbParen (6 < p) (showbPrec 5 a <> "*" <> showbPrec 6 b)
    showbPrec p (Div a b)   = showbParen (6 < p) (showbPrec 5 a <> "/" <> showbPrec 6 b)
    showbPrec p (Expo a b)  = showbParen (7 < p) (showbPrec 5 a <> "^" <> showbPrec 7 b)
    showbPrec p (Log a b)   = "log[" <> showb a <> "](" <> showb b <> ")"

instance TextShow RealCyclotomic where
    showb = fromText . pack . show

-- erstatter alle variabler med sin nye verdi.
betaReduce :: Expr -> [(Text, Expr)] -> Expr
betaReduce (Var v) args = head [ value | (name, value) <- args, v == name ]
betaReduce (Num n) args = Num n
betaReduce (Add a b) args = Add (betaReduce a args) (betaReduce b args)
betaReduce (USub a) args = USub (betaReduce a args)
betaReduce (BSub a b) args = BSub (betaReduce a args) (betaReduce b args)
betaReduce (Mult a b) args = Mult (betaReduce a args) (betaReduce b args)
betaReduce (Div a b) args = Div (betaReduce a args) (betaReduce b args)
betaReduce (Expo a b) args = Expo (betaReduce a args) (betaReduce b args)
betaReduce (Log a b) args = Log (betaReduce a args) (betaReduce b args)

isConstant :: Expr -> Bool
isConstant (Var _) = False
isConstant (Num _) = True
isConstant (Add a b) = isConstant a && isConstant b
isConstant (BSub a b) = isConstant a && isConstant b
isConstant (USub a) = isConstant a
isConstant (Mult a b) = isConstant a && isConstant b
isConstant (Div a b) = isConstant a && isConstant b
isConstant (Log a b) = isConstant a && isConstant b
isConstant (Expo a b) = isConstant a && isConstant b

-- Kræsjer om uttrykket ikke er en konstant.
evalConstant :: Expr -> RealCyclotomic
evalConstant (Var _) = error "Unexpected variable in 'constant'"
evalConstant (Num a) = a
evalConstant (Add a b) = evalConstant a + evalConstant b
evalConstant (BSub a b) = evalConstant a - evalConstant b
evalConstant (USub a)  = - evalConstant a
evalConstant (Mult a b) = evalConstant a * evalConstant b
evalConstant (Div a b) = evalConstant a / evalConstant b
evalConstant (Log a b) = realToRat $ logBase (toReal (evalConstant a)) (toReal (evalConstant b))
evalConstant (Expo a b) = realToRat $ toReal (evalConstant a) ** toReal (evalConstant b)


evalFunction :: Function -> [Argument] -> Either Text RealCyclotomic
evalFunction (Function _ params ex) args | params == map fst args = Right $ evalFunction' ex args
                                         | otherwise = Left ("Function arguments did not match function parameters: " <> toText (showb (map fst args)) <> " vs " <> toText (showb params))
    where
        evalFunction' :: Expr -> [Argument] -> RealCyclotomic
        evalFunction' (Var v) params = head [ value | (name, value) <- params, v == name ]
        evalFunction' (Num a) _ = a
        evalFunction' (Add a b) params = evalFunction' a params + evalFunction' b params
        evalFunction' (BSub a b) params = evalFunction' a params - evalFunction' b params
        evalFunction' (USub a) params = - evalFunction' a params
        evalFunction' (Mult a b) params = evalFunction' a params * evalFunction' b params
        evalFunction' (Div a b) params = evalFunction' a params / evalFunction' b params
        evalFunction' (Log a b) params = logBase (evalFunction' a params) (evalFunction' b params)
        evalFunction' (Expo a b) params = evalFunction' a params ** evalFunction' b params


