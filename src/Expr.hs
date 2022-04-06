--https://www.mdpi.com/2073-8994/10/7/285
--A Step-by-Step Solution Methodology for Mathematical Expressions 

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Expr (Expr(..), Argument, Parameter, Function(Function), UnaryFunction(..), BinaryFunction(..), 
    PrefixFunction(..), InfixFunction(..), bFunctions, uFunctions, bFuncFromName, uFuncFromName, bFuncFromConstr, uFuncFromConstr, isConstant, evalConstant,
     evalFunction, betaReduce, uFuncNames, bFuncNames) where

import Data.Number.RealCyclotomic (RealCyclotomic, toReal, toRat)
import Data.Text (Text, unpack, pack, toLower)
import Data.List (intercalate)
import TextShow (TextShow(..), showbParen, showb, fromText, toText, toString)
import Data.Maybe (listToMaybe)

import Utility (realToRat, e)


data Expr n = UFunc UnaryFunction (Expr n)
            | BFunc BinaryFunction (Expr n) (Expr n)
            | Num n
            | Var Text
            | Const Text n
    deriving Eq

data Function n = Function Text [Parameter] (Expr n)

type Argument n = (Text, n)
type Parameter = Text

data UnaryFunction = Sin | Cos | Tan | USub | Ceiling | Floor | Sqrt | Succ | Pred | Abs
    deriving (Eq, Show)

data BinaryFunction = Prefix PrefixFunction
                    | Infix  InfixFunction
    deriving (Eq, Show)

data PrefixFunction = Log | Max | Min
    deriving (Eq, Show)

data InfixFunction  = Add | BSub | Mult | Div | Expo
    deriving (Eq, Show)

uFunctions :: RealFloat n => [(UnaryFunction, Text, n -> n)]
uFunctions = [
        (Sin, "sin", sin),
        (Cos, "cos", cos),
        (Tan, "tan", tan),
        (USub, "-", negate),
        (Ceiling, "ceiling", fromIntegral . ceiling),
        (Floor, "floor", fromIntegral . floor),
        (Sqrt, "sqrt", sqrt),
        (Succ, "succ", (+1)),
        (Pred, "pred", (-) 1),
        (Abs, "abs", abs)
    ]

bFunctions :: RealFloat n => [(BinaryFunction, Text, n -> n -> n)]
bFunctions = [
        (Infix Add, "+", (+)),
        (Infix BSub, "-", (-)),
        (Infix Mult, "*", (*)),
        (Infix Div, "/", (/)),
        (Prefix Log, "log", logBase),
        (Infix Expo, "^", (**)),
        (Prefix Max, "max", max),
        (Prefix Min, "min", min)
    ]

infixPrecedence :: InfixFunction -> Int
infixPrecedence BSub = 3
infixPrecedence Add  = 4
infixPrecedence Div  = 5
infixPrecedence Mult = 6
infixPrecedence Expo = 7

uFuncNames :: [Text]
uFuncNames = [name | (_, name, _) <- uFunctions]

bFuncNames :: [Text]
bFuncNames = [name | (_, name, _) <- bFunctions]

uFuncFromName :: RealFloat n => Text -> Maybe (UnaryFunction, Text, n -> n)
uFuncFromName t = listToMaybe [ f | f@(con, name, func) <- uFunctions, name == toLower t ]

bFuncFromName :: RealFloat n => Text-> Maybe (BinaryFunction, Text, n -> n -> n)
bFuncFromName t = listToMaybe [ f | f@(con, name, func) <- bFunctions, name == toLower t ]

uFuncFromConstr :: RealFloat n => UnaryFunction -> (UnaryFunction, Text, n -> n)
uFuncFromConstr c = head [ f | f@(con, name, func) <- uFunctions, con == c ]

bFuncFromConstr :: RealFloat n => BinaryFunction -> (BinaryFunction, Text, n -> n -> n)
bFuncFromConstr c = head [ f | f@(con, name, func) <- bFunctions, con == c ]


instance (RealFloat n, TextShow n) => Show (Function n) where
    show (Function name params expr) = unpack name <> "(" <> intercalate "," (map unpack params) <> ") = " <> toString (showb expr)

instance Eq (Function n) where
    (Function name1 _ _) == (Function name2 _ _) = name1 == name2


instance (RealFloat n, TextShow n) => TextShow (Expr n) where
    showbPrec p (Const name _) = fromText name
    showbPrec p (Num  a)    = showb a
    showbPrec p (Var v)     = fromText v
    showbPrec p (UFunc USub a) = showbParen (5 < p) ("-" <> showbPrec 5 a)
    showbPrec p (UFunc c a) = let (_, name, _) = uFuncFromConstr c in fromText name <> "(" <> showb a <> ")"
    showbPrec p (BFunc (Prefix Log) a b) | a == Num e = "ln(" <> showb b <> ")"
                                         | otherwise = "log[" <> showb a <> "](" <> showb b <> ")"
    showbPrec p (BFunc (Prefix c) a b) = let (_, name, _) = bFuncFromConstr (Prefix c) in fromText name <> "(" <> showb a <> ", " <> showb b <> ")"
    showbPrec p (BFunc (Infix c) a b) = let (_, op, _) = bFuncFromConstr (Infix c) 
                                            opPrec = infixPrecedence c
                                        in  showbParen (opPrec < p) (showbPrec opPrec a <> fromText op <> showbPrec opPrec b)

instance (TextShow n, RealFloat n, Show n) => Show (Expr n) where
    show = toString . showb

instance Num n => Num (Expr n) where
    (+) = BFunc (Infix Add)
    (-) = BFunc (Infix BSub)
    (*) = BFunc (Infix Mult)
    signum = undefined 
    abs = UFunc Abs
    fromInteger = Num . fromInteger

instance Fractional n => Fractional (Expr n) where
    (/) = BFunc (Infix Div)
    fromRational = Num . fromRational
    

-- erstatter alle variabler med sin nye verdi.
betaReduce :: Expr n -> [(Text, Expr n)] -> Expr n
betaReduce (Const name n) _    = Const name n
betaReduce (Num n)        _    = Num n
betaReduce (Var v)        args = head [ value | (name, value) <- args, v == name ]
betaReduce (UFunc c a)    args = UFunc c (betaReduce a args)
betaReduce (BFunc c a b)  args = BFunc c (betaReduce a args) (betaReduce b args)

isConstant :: Expr n -> Bool
isConstant (Var _) = False
isConstant (Num _) = True
isConstant (Const _ _) = True
isConstant (UFunc _ a) = isConstant a
isConstant (BFunc _ a b) = isConstant a && isConstant b

-- Kræsjer om uttrykket ikke er en konstant.
evalConstant :: RealFloat n => Expr n -> n
evalConstant (Var _) = error "Unexpected variable in 'constant'"
evalConstant (Num a) = a
evalConstant (Const _ n) = n
evalConstant (UFunc c a) = let (_, _, f) = uFuncFromConstr c in f (evalConstant a)
evalConstant (BFunc c a b) = let (_, _, f) = bFuncFromConstr c in f (evalConstant a) (evalConstant b)

evalFunction :: RealFloat n => Function n -> [Argument n] -> Either Text n
evalFunction (Function _ params ex) args | params == map fst args = Right $ evalFunction' ex args
                                         | otherwise = Left ("Function arguments did not match function parameters: " <> toText (showb (map fst args)) <> " vs " <> toText (showb params))
    where
        evalFunction' :: RealFloat n => Expr n -> [Argument n] -> n
        evalFunction' (Num a)        _ = a
        evalFunction' (Const name n) _ = n
        evalFunction' (Var v)        params = head [ value | (name, value) <- params, v == name ]
        evalFunction' (UFunc c a)    params = let (_, _, f) = uFuncFromConstr c in f (evalFunction' a params)
        evalFunction' (BFunc c a b)  params = let (_, _, f) = bFuncFromConstr c in f (evalFunction' a params) (evalFunction' b params)



