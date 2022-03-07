--https://www.mdpi.com/2073-8994/10/7/285
--A Step-by-Step Solution Methodology for Mathematical Expressions 

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Expr (Expr(..), Argument, Parameter, Function(Function), UnaryFunction(..), BinaryFunction(..), 
    PrefixFunction(..), InfixFunction(..), bFunctions, uFunctions, bFuncFromName, uFuncFromName, isConstant, evalConstant,
     evalFunction, betaReduce, uFuncNames, bFuncNames) where

import Data.Number.RealCyclotomic (RealCyclotomic, toReal, toRat)
import Data.Text (Text, unpack, pack, toLower)
import Data.List (intercalate)
import TextShow (TextShow(..), showbParen, showb, fromText, toText, toString)
import Data.Maybe (listToMaybe)


import Utility (realToRat, e)


-- 2 + 3 * 4^5 = Add (Num 2) (Mult (Num 3) (Expo (Num 4) (Num 5)))

data Expr = UFunc UnaryFunction Expr
          | BFunc BinaryFunction Expr Expr
          | Num RealCyclotomic
          | Var Text
    deriving (Show, Eq)

type Argument = (Text, RealCyclotomic)
type Parameter = Text

data UnaryFunction = Sin | Cos | Tan | USub -- | Ceiling | Floor
    deriving (Eq, Show)

data BinaryFunction = Prefix PrefixFunction
                    | Infix  InfixFunction
    deriving (Eq, Show)

data PrefixFunction = Log -- | Max | Min
    deriving (Eq, Show)

data InfixFunction  = Add | BSub | Mult | Div | Expo
    deriving (Eq, Show)

uFunctions :: [(UnaryFunction, Text, RealCyclotomic -> RealCyclotomic)]
uFunctions = [
        (Sin, "sin", sin),
        (Cos, "cos", cos),
        (Tan, "tan", tan),
        (USub, "-", negate)
    ]

bFunctions :: [(BinaryFunction, Text, RealCyclotomic -> RealCyclotomic -> RealCyclotomic)]
bFunctions = [
        (Infix Add, "+", (+)),
        (Infix BSub, "-", (-)),
        (Infix Mult, "*", (*)),
        (Infix Div, "/", (/)),
        (Prefix Log, "log", logBase),
        (Infix Expo, "^", (**)) --,
        -- (Prefix Max, "max", \x y -> realToRat $ max (toReal x) (toReal y)),
        -- (Prefix Min, "min", \x y -> realToRat $ min (toReal x) (toReal y))
    ]

infixPrecedence :: InfixFunction -> Int
infixPrecedence Add  = 5
infixPrecedence BSub = 5
infixPrecedence Mult = 6
infixPrecedence Div  = 6
infixPrecedence Expo = 7

uFuncNames :: [Text]
uFuncNames = [name | (_, name, _) <- uFunctions]

bFuncNames :: [Text]
bFuncNames = [name | (_, name, _) <- bFunctions]

uFuncFromName :: Text -> Maybe (UnaryFunction, Text, RealCyclotomic -> RealCyclotomic)
uFuncFromName t = listToMaybe [ f | f@(con, name, func) <- uFunctions, name == toLower t ]

bFuncFromName :: Text-> Maybe (BinaryFunction, Text, RealCyclotomic -> RealCyclotomic -> RealCyclotomic)
bFuncFromName t = listToMaybe [ f | f@(con, name, func) <- bFunctions, name == toLower t ]

uFuncFromConstr :: UnaryFunction -> (UnaryFunction, Text, RealCyclotomic -> RealCyclotomic)
uFuncFromConstr c = head [ f | f@(con, name, func) <- uFunctions, con == c ]

bFuncFromConst :: BinaryFunction -> (BinaryFunction, Text, RealCyclotomic -> RealCyclotomic -> RealCyclotomic)
bFuncFromConst c = head [ f | f@(con, name, func) <- bFunctions, con == c ]


data Function = Function Text [Parameter] Expr

instance Show Function where
    show (Function name params expr) = unpack name <> "(" <> intercalate "," (map unpack params) <> ") = " <> toString (showb expr)

instance TextShow Expr where
    showbPrec p (Num  a)    = showb a
    showbPrec p (Var v)     = fromText v
    showbPrec p (UFunc USub a) = showbParen (5 < p) ("-" <> showbPrec 5 a)
    showbPrec p (UFunc c a) = let (_, name, _) = uFuncFromConstr c in fromText name <> "(" <> showb a <> ")"
    showbPrec p (BFunc (Prefix Log) a b) | a == Num e = "ln(" <> showb b <> ")"
                                         | otherwise = "log[" <> showb a <> "](" <> showb b <> ")"
    showbPrec p (BFunc (Prefix c) a b) = let (_, name, _) = bFuncFromConst (Prefix c) in fromText name <> "(" <> showb a <> ", " <> showb b <> ")"
    showbPrec p (BFunc (Infix c) a b) = let (_, op, _) = bFuncFromConst (Infix c) 
                                            opPrec = infixPrecedence c
                                        in  showbParen (opPrec < p) (showbPrec opPrec a <> fromText op <> showbPrec opPrec b)


instance TextShow RealCyclotomic where
    showb = fromText . pack . show

-- erstatter alle variabler med sin nye verdi.
betaReduce :: Expr -> [(Text, Expr)] -> Expr
betaReduce (Var v) args = head [ value | (name, value) <- args, v == name ]
betaReduce (Num n) args = Num n
betaReduce (UFunc c a) args = UFunc c (betaReduce a args)
betaReduce (BFunc c a b) args = BFunc c (betaReduce a args) (betaReduce b args)

isConstant :: Expr -> Bool
isConstant (Var _) = False
isConstant (Num _) = True
isConstant (UFunc _ a) = isConstant a
isConstant (BFunc _ a b) = isConstant a && isConstant b

-- Kræsjer om uttrykket ikke er en konstant.
evalConstant :: Expr -> RealCyclotomic
evalConstant (Var _) = error "Unexpected variable in 'constant'"
evalConstant (Num a) = a
evalConstant (UFunc c a) = let (_, _, f) = uFuncFromConstr c in f (evalConstant a)
evalConstant (BFunc c a b) = let (_, _, f) = bFuncFromConst c in f (evalConstant a) (evalConstant b)

evalFunction :: Function -> [Argument] -> Either Text RealCyclotomic
evalFunction (Function _ params ex) args | params == map fst args = Right $ evalFunction' ex args
                                         | otherwise = Left ("Function arguments did not match function parameters: " <> toText (showb (map fst args)) <> " vs " <> toText (showb params))
    where
        evalFunction' :: Expr -> [Argument] -> RealCyclotomic
        evalFunction' (Var v) params = head [ value | (name, value) <- params, v == name ]
        evalFunction' (Num a) _ = a
        evalFunction' (UFunc c a) params = let (_, _, f) = uFuncFromConstr c in f (evalFunction' a params)
        evalFunction' (BFunc c a b) params = let (_, _, f) = bFuncFromConst c in f (evalFunction' a params) (evalFunction' b params)

