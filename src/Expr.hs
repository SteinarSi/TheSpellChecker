--https://www.mdpi.com/2073-8994/10/7/285
--A Step-by-Step Solution Methodology for Mathematical Expressions 

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Expr (Expr(..), Argument, Parameter, Constant(..), Function(Function), UnaryFunction(..), BinaryFunction(..),
    PrefixFunction(..), InfixFunction(..), bFunctions, uFunctions, bFuncFromName, uFuncFromName, bFuncFromConstr, uFuncFromConstr, isConstant,
        betaReduce, uFuncNames, bFuncNames, debug, eval) where

import Data.Number.RealCyclotomic (RealCyclotomic, toReal, toRat)
import Data.Text (Text, unpack, pack, toLower)
import qualified Data.Text as T
import Data.List (intercalate, foldr1, intersperse)
import TextShow (TextShow(..), showbParen, showb, fromText, toText, toString)
import Data.Maybe (listToMaybe)

import Utility (realToRat, Debug(debug))


data Expr n = UFunc UnaryFunction (Expr n)
            | BFunc BinaryFunction (Expr n) (Expr n)
            | FFunc (Function n) [Expr n]
            | Z Integer
            | R n
            | BoundedVar Text
            | FreeVar Text (Expr n)
            | Const Constant
    deriving Eq

data Function n = Function Text [Parameter] (Expr n)

type Argument n = Expr n
type Parameter = Text

data Constant = Pi | E
    deriving (Eq, Show)

data UnaryFunction = Sin | Cos | Tan | USub | Ceiling | Floor | Sqrt | Succ | Pred | Abs | Asin | Acos | Atan | Sinh | Cosh | Tanh
                   | Asinh | Acosh | Atanh
    deriving (Eq, Show)

data BinaryFunction = Prefix PrefixFunction
                    | Infix  InfixFunction
    deriving (Eq, Show)

data PrefixFunction = Log | Max | Min
    deriving (Eq, Show)

data InfixFunction  = Add | BSub | Mult | Div | Expo
    deriving (Eq, Show)

constants :: Floating n => [(Constant, Text, n)]
constants = [
        (Pi, "pi", pi),
        (E, "e", 2.71828182845904523536028747)
    ]

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
        (Pred, "pred", subtract 1),
        (Abs, "abs", abs),
        (Asin, "asin", asin),
        (Acos, "acos", acos),
        (Atan, "atan", atan),
        (Sinh, "sinh", sinh),
        (Cosh, "cosh", cosh),
        (Tanh, "tanh", tanh),
        (Asinh, "asinh", asinh),
        (Acosh, "acosh", acosh),
        (Atanh, "atanh", atanh)
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

constantFromConstr :: Floating n => Constant -> (Constant, Text, n)
constantFromConstr c = head [ f | f@(con, _, _)<-constants, con == c ]

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


instance (Floating n, TextShow n) => Show (Function n) where
    show (Function name params expr) = unpack name <> "(" <> intercalate "," (map unpack params) <> ") = " <> toString (showb expr)

instance Eq (Function n) where
    (Function name1 _ _) == (Function name2 _ _) = name1 == name2

-- debug (Function name params expr) = unpack name <> "(" <> intercalate "," (map unpack params) <> ") = " <> debug expr
instance (Floating n, TextShow n) => TextShow (Expr n) where
    showbPrec p (FFunc (Function name [] _) _) = fromText name
    showbPrec p (FFunc (Function name _ _) args) = fromText name <> "(" <> foldr1 ((<>) . (<> ",")) (map showb args) <> ")"
    showbPrec p (Const c)   = let (_, name, _) = constantFromConstr c in fromText name
    showbPrec p (Z n)     = showb n
    showbPrec p (R  a)    = showb a
    showbPrec p (BoundedVar v)     = fromText v
    showbPrec p (FreeVar n _) = fromText n
    showbPrec p (UFunc USub a) = showbParen (5 < p) ("-" <> showbPrec 5 a)
    showbPrec p (UFunc c a) = let (_, name, _) = uFuncFromConstr c in fromText name <> "(" <> showb a <> ")"
    showbPrec p (BFunc (Prefix Log) (Const E) b) = "ln(" <> showb b <> ")"
    showbPrec p (BFunc (Prefix Log) a b) = "log[" <> showb a <> "](" <> showb b <> ")"
    showbPrec p (BFunc (Prefix c) a b) = let (_, name, _) = bFuncFromConstr (Prefix c) in fromText name <> "(" <> showb a <> ", " <> showb b <> ")"
    showbPrec p (BFunc (Infix c) a b) = let (_, op, _) = bFuncFromConstr (Infix c)
                                            opPrec = infixPrecedence c
                                        in  showbParen (opPrec < p) (showbPrec opPrec a <> fromText op <> showbPrec opPrec b)

instance (TextShow n, Floating n) => Show (Expr n) where
    show = toString . showb

instance Num n => Num (Expr n) where
    (+) = BFunc (Infix Add)
    (-) = BFunc (Infix BSub)
    (*) = BFunc (Infix Mult)
    signum = undefined
    abs = UFunc Abs
    fromInteger = Z . fromInteger

instance Fractional n => Fractional (Expr n) where
    (/) = BFunc (Infix Div)
    fromRational = R . fromRational

instance Floating n => Floating (Expr n) where
    pi = R pi
    exp = BFunc (Infix Expo) (Const E)
    log = BFunc (Infix Expo) (Const E)
    sin   = UFunc Sin
    cos   = UFunc Cos
    tan   = UFunc Tan
    asin  = UFunc Asin
    acos  = UFunc Acos
    atan  = UFunc Atan
    sinh  = UFunc Sinh
    cosh  = UFunc Cosh
    tanh  = UFunc Tanh
    asinh = UFunc Asinh
    acosh = UFunc Acosh
    atanh = UFunc Atanh

instance Debug UnaryFunction where
    debug = show

instance Debug InfixFunction where
    debug = show

instance Debug PrefixFunction where
    debug = show

instance Show n => Debug (Expr n) where
    debug (FFunc (Function name _ _) args) = unpack name <> "(" <> intercalate "," (map debug args) <> ")"
    debug (Const c) = show c
    debug (Z z) = show z
    debug (R r) = show r
    debug (UFunc c e) = "(" <> debug c <> " " <> debug e <> ")"
    debug (UFunc c e) = "(" <> debug c <> " " <> debug e <> ")"
    debug (BFunc (Infix c) a b) = let (_, n, _) = bFuncFromConstr (Infix c)
                                  in "(" <> debug a <> " " <> unpack n <> " " <> debug b <>")"
    debug (BFunc (Prefix c) a b) = "(" <> debug c <> " " <> debug a <> " " <> debug b <> ")"
    debug (BoundedVar x) = "(Var " <> unpack x <> ")"
    debug (FreeVar v ex) = "(Var " <> unpack v <> "=" <> debug ex <> ")"

instance Show n => Debug (Function n) where
    debug (Function name params expr) = unpack name <> "(" <> intercalate "," (map unpack params) <> ") = " <> debug expr

-- erstatter alle variabler med sin nye verdi.
betaReduce :: Expr n -> [(Text, Expr n)] -> Expr n
betaReduce = betaReduce' []
    where
        betaReduce' :: [Text] -> Expr n -> [(Text, Expr n)] -> Expr n
        betaReduce' bounded (Const c)      _    = Const c
        betaReduce' bounded (Z z)          _    = Z z
        betaReduce' bounded (R r)          _    = R r
        betaReduce' bounded (FreeVar v ex) args = FreeVar v ex
        betaReduce' bounded (BoundedVar v) args | v `elem` bounded = BoundedVar v
                                                | otherwise = head [ value | (name, value) <- args, v == name ]
        betaReduce' bounded (UFunc c a)    args = UFunc c (betaReduce' bounded a args)
        betaReduce' bounded (BFunc c a b)  args = BFunc c (betaReduce' bounded a args) (betaReduce' bounded b args)
        betaReduce' bounded (FFunc (Function name params expr) funcargs) args = 
            FFunc (Function name params (betaReduce' (params ++ bounded) expr args)) (map (\a -> betaReduce' bounded a args) funcargs)

isConstant :: Expr n -> Bool
isConstant (BoundedVar _) = False
isConstant (FreeVar v ex) = True
isConstant (Z _)          = True
isConstant (R _)          = True
isConstant (Const _)      = True
isConstant (UFunc _ a)    = isConstant a
isConstant (BFunc _ a b)  = isConstant a && isConstant b
isConstant (FFunc (Function _ params expr) args) = isConstant (betaReduce expr (zip params args))


-- Kræsjer om uttrykket ikke er en konstant.
eval :: (Show n, RealFloat n) => Expr n -> Either Text n
eval (BoundedVar v) = Left ("Unexpected variable in 'constant': " <> v)
eval (FreeVar v ex) = eval ex
eval (Z z)          = Right (fromInteger z)
eval (R r)          = Right r
eval (Const c)      = let (_, _, n) = constantFromConstr c in Right n
eval (UFunc c a)    = let (_, _, f) = uFuncFromConstr c in f <$> eval a
eval (BFunc (Prefix Log) a b) = case (eval a, eval b) of
    (Right x, Right y) | x <= 0 -> Left (pack ("The base of a logarithm must be strictly positive, but was: " <> show x))
                       | y <= 0 -> Left (pack ("The argument to a log has to be strictly positive, but was: " <> show y))
                       | otherwise -> let (_, _, f) = bFuncFromConstr (Prefix Log) in Right (f x y)
    (Left err, _) -> Left err
    (_, Left err) -> Left err
eval (BFunc c a b)  = let (_, _, f) = bFuncFromConstr c in f <$> eval a <*> eval b
eval (FFunc f@(Function name params expr) args) = eval (betaReduce expr (zip params args))
