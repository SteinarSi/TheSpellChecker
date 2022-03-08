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


-- 2 + 3 * 4^5 = Add (Num 2) (Mult (Num 3) (Expo (Num 4) (Num 5)))

data Expr n = UFunc UnaryFunction (Expr n)
            | BFunc BinaryFunction (Expr n) (Expr n)
            | Num n
            | Var Text
    deriving (Show, Eq)

data Function n = Function Text [Parameter] (Expr n)

type Argument n = (Text, n)
type Parameter = Text

data UnaryFunction = Sin | Cos | Tan | USub | Ceiling | Floor
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
        (Floor, "floor", fromIntegral . floor)
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
infixPrecedence Add  = 3
infixPrecedence BSub = 4
infixPrecedence Mult = 5
infixPrecedence Div  = 6
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

instance (RealFloat n, TextShow n) => TextShow (Expr n) where
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


instance TextShow RealCyclotomic where
    showb = fromText . pack . show

-- erstatter alle variabler med sin nye verdi.
betaReduce :: Expr n -> [(Text, Expr n)] -> Expr n
betaReduce (Var v) args = head [ value | (name, value) <- args, v == name ]
betaReduce (Num n) args = Num n
betaReduce (UFunc c a) args = UFunc c (betaReduce a args)
betaReduce (BFunc c a b) args = BFunc c (betaReduce a args) (betaReduce b args)

isConstant :: Expr n -> Bool
isConstant (Var _) = False
isConstant (Num _) = True
isConstant (UFunc _ a) = isConstant a
isConstant (BFunc _ a b) = isConstant a && isConstant b

-- Kræsjer om uttrykket ikke er en konstant.
evalConstant :: RealFloat n => Expr n -> n
evalConstant (Var _) = error "Unexpected variable in 'constant'"
evalConstant (Num a) = a
evalConstant (UFunc c a) = let (_, _, f) = uFuncFromConstr c in f (evalConstant a)
evalConstant (BFunc c a b) = let (_, _, f) = bFuncFromConstr c in f (evalConstant a) (evalConstant b)

evalFunction :: RealFloat n => Function n -> [Argument n] -> Either Text n
evalFunction (Function _ params ex) args | params == map fst args = Right $ evalFunction' ex args
                                         | otherwise = Left ("Function arguments did not match function parameters: " <> toText (showb (map fst args)) <> " vs " <> toText (showb params))
    where
        evalFunction' :: RealFloat n => Expr n -> [Argument n] -> n
        evalFunction' (Var v) params = head [ value | (name, value) <- params, v == name ]
        evalFunction' (Num a) _ = a
        evalFunction' (UFunc c a) params = let (_, _, f) = uFuncFromConstr c in f (evalFunction' a params)
        evalFunction' (BFunc c a b) params = let (_, _, f) = bFuncFromConstr c in f (evalFunction' a params) (evalFunction' b params)

