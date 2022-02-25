--https://www.mdpi.com/2073-8994/10/7/285
--A Step-by-Step Solution Methodology for Mathematical Expressions 

{-# LANGUAGE OverloadedStrings #-}

module Expr (Expr(..), Argument, Parameter, Function(Function), realToRat) where

import Data.Number.RealCyclotomic (RealCyclotomic, toReal, toRat)
import Data.Text (Text, unpack, pack)

import TextShow (TextShow(..), showbParen, showb, fromText)


-- 2 + 3 * 4^5 = Add (Num 2) (Mult (Num 3) (Expo (Num 4) (Num 5)))

data Expr = Add Expr Expr
          | BSub Expr Expr --binær minus
          | USub Expr --unær minus
          | Mult Expr Expr
          | Div Expr Expr
          | Log Expr Expr -- logf(g), der både f og g er funksjoner gitt av noen parametre
          | Expo Expr Expr
          | Num RealCyclotomic
          | Var Text
    deriving (Show, Eq)

type Argument = (Text, RealCyclotomic)
type Parameter = Text

data Function = Function Text [Parameter] Expr
    deriving Show



 ---TODOOOOOOO
instance TextShow Expr where
    showbPrec p (Num  a)   = showb a
    showbPrec p (Add  a b) = showbParen (5 < p) (showbPrec p a <> "+" <> showbPrec 5 b)
    showbPrec p (Mult a b) = showbParen (6 < p) (showbPrec p a <> "*" <> showbPrec 6 b)
    showbPrec p (Expo a b) = showbParen (7 < p) (showbPrec p a <> "^" <> showbPrec 7 b)

instance TextShow RealCyclotomic where
    showb = fromText . pack . show

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


-- Kræsjer om den ikke får alle parametrene den skal.

--evalFunction :: Function -> [Argument] -> Either Text RealCyclotomic 
--evalFunction = undefined 

evalFunction :: Expr -> [Argument] -> RealCyclotomic 
evalFunction (Var v) params = head [ value | (name, value) <- params, v == name ]
evalFunction (Num a) _ = a
evalFunction (Add a b) params = evalFunction a params + evalFunction b params
evalFunction (BSub a b) params = evalFunction a params - evalFunction b params
evalFunction (USub a) params = - evalFunction a params
evalFunction (Mult a b) params = evalFunction a params * evalFunction b params
evalFunction (Div a b) params = evalFunction a params / evalFunction b params
evalFunction (Log a b) params = realToRat $ logBase (toReal (evalFunction a params)) (toReal (evalFunction b params))
evalFunction (Expo a b) params = realToRat $ toReal (evalFunction a params) ** toReal (evalFunction b params)

realToRat :: Real a => a -> RealCyclotomic
realToRat = fromRational . realToFrac
