--https://www.mdpi.com/2073-8994/10/7/285
--A Step-by-Step Solution Methodology for Mathematical Expressions 

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Expr (Expr(..), Argument, Parameter, Constant(..), Function(Function), UnaryFunction(..), BinaryFunction(..),
    PrefixFunction(..), InfixFunction(..), bFunctions, uFunctions, bFuncFromName, uFuncFromName, bFuncFromConstr, uFuncFromConstr, isConstant,
        betaReduce, uFuncNames, bFuncNames, debug, eval, inline) where

import Data.Number.RealCyclotomic (RealCyclotomic, toReal, toRat)
import Data.Text (Text, unpack, pack, toLower)
import qualified Data.Text as T
import Data.List (intercalate, foldr1, intersperse)
import TextShow (TextShow(..), showbParen, showb, fromText, toText, toString)
import Data.Maybe (listToMaybe)
import Data.Bool (bool)

import Test.LeanCheck

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

type UDomain a = Maybe (a -> Maybe Text)
type BDomain a b = Maybe (a -> b -> Maybe Text)

constants :: Floating n => [(Constant, Text, n)]
constants = [
        (Pi, "pi", pi),
        (E, "e", 2.71828182845904523536028747)
    ]

uFunctions :: (RealFloat n, Show n) => [(UnaryFunction, Text, n -> n, UDomain n)]
uFunctions = [
        (Sin, "sin", sin, Nothing),
        (Cos, "cos", cos, Nothing),
        (Tan, "tan", tan, Nothing),
        (USub, "-", negate, Nothing),
        (Ceiling, "ceiling", fromIntegral . ceiling, Nothing),
        (Floor, "floor", fromIntegral . floor, Nothing),
        (Sqrt, "sqrt", sqrt, Just $ bool (Just "negative numbers have noe real roots (and no, I'm not implementing complex numbers)") Nothing . (>=0)),
        (Succ, "succ", (+1), Nothing),
        (Pred, "pred", subtract 1, Nothing),
        (Abs, "abs", abs, Nothing),
        (Asin, "asin", asin, Just $ \a -> bool (Just $ pack ("asin() is only defined on -1 <= x <= 1, but the argument was " <> show a <> ".")) Nothing (abs a <= 1)),
        (Acos, "acos", acos, Just $ \a -> bool (Just $ pack ("acos() is only defined on -1 <= x <= 1, but the argument was " <> show a <> ".")) Nothing (abs a <= 1)),
        (Atan, "atan", atan, Nothing),
        (Sinh, "sinh", sinh, Nothing),
        (Cosh, "cosh", cosh, Nothing),
        (Tanh, "tanh", tanh, Nothing),
        (Asinh, "asinh", asinh, Nothing),
        (Acosh, "acosh", acosh, Just $ \a -> bool (Just $ pack ("acosh() is only defined for x >= 1, but the argument was " <> show a <> ".")) Nothing (a>=1)),
        (Atanh, "atanh", atanh, Just $ \a -> bool (Just $ pack $ "atanh() is only defined for -1 < x < 1, but the argument was " <> show a <> ".") Nothing (a > -1 && a < 1))
    ]

bFunctions :: (RealFloat n, Show n) => [(BinaryFunction, Text, n -> n -> n, BDomain n n)]
bFunctions = [
        (Infix Add, "+", (+), Nothing),
        (Infix BSub, "-", (-), Nothing),
        (Infix Mult, "*", (*), Nothing),
        (Infix Div, "/", (/), Just (\_ b -> bool Nothing (Just "Error: Divison by zero.") (b==0))),
        (Prefix Log, "log", logBase, Just (\a b -> case (a, b) of
                                            (1, _) -> Just $ pack "Error: The base of a logarithm cannot be 1."
                                            (a, b) | a <= 0 -> Just $ pack ("Error: The base of a logarithm must be strictly positive, but was: " <> show a <> ".")
                                                   | b <= 0 -> Just $ pack ("Error: The argument to a log has to be strictly positive, but was: " <> show b <> ".")
                                                   | otherwise -> Nothing)),
        (Infix Expo, "^", (**), Nothing),
        (Prefix Max, "max", max, Nothing),
        (Prefix Min, "min", min, Nothing)
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
uFuncNames = [name | (_, name, _, _) <- uFunctions]

bFuncNames :: [Text]
bFuncNames = [name | (_, name, _, _) <- bFunctions]

uFuncFromName :: (Show n, RealFloat n) => Text -> Maybe (UnaryFunction, Text, n -> n, UDomain n)
uFuncFromName t = listToMaybe [ f | f@(_, name, _, _) <- uFunctions, name == toLower t ]

bFuncFromName :: (Show n, RealFloat n) => Text-> Maybe (BinaryFunction, Text, n -> n -> n, BDomain n n)
bFuncFromName t = listToMaybe [ f | f@(_, name, _, _) <- bFunctions, name == toLower t ]

uFuncFromConstr :: (Show n, RealFloat n) => UnaryFunction -> (UnaryFunction, Text, n -> n, UDomain n)
uFuncFromConstr c = head [ f | f@(con, _, _, _) <- uFunctions, con == c ]

bFuncFromConstr :: (Show n, RealFloat n) => BinaryFunction -> (BinaryFunction, Text, n -> n -> n, BDomain n n)
bFuncFromConstr c = head [ f | f@(con, _, _, _) <- bFunctions, con == c ]


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
    showbPrec p (UFunc c a) = let (_, name, _, _) = uFuncFromConstr c in fromText name <> "(" <> showb a <> ")"
    showbPrec p (BFunc (Prefix Log) (Const E) b) = "ln(" <> showb b <> ")"
    showbPrec p (BFunc (Prefix Log) a b) = "log[" <> showb a <> "](" <> showb b <> ")"
    showbPrec p (BFunc (Prefix c) a b) = let (_, name, _, _) = bFuncFromConstr (Prefix c) in fromText name <> "(" <> showb a <> ", " <> showb b <> ")"
    showbPrec p (BFunc (Infix c) a b) = let (_, op, _, _) = bFuncFromConstr (Infix c)
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
    (**) a b = BFunc (Infix Expo) a b
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

instance Eq n => Ord (Expr n) where
    (<=) (Z _) _ = True
    (<=) (R _) (Z _) = False
    (<=) (R _) _ = True
    (<=) (Const _) (Z _) = False
    (<=) (Const _) (R _) = False
    (<=) (Const _) _ = True
    (<=) (BoundedVar _) (Z _) = False
    (<=) (BoundedVar _) (R _) = False
    (<=) (BoundedVar _) (Const _) = False
    (<=) (BoundedVar _) _ = True
    (<=) (FreeVar _ _) (Z _) = False
    (<=) (FreeVar _ _) (R _) = False
    (<=) (FreeVar _ _) (Const _) = False
    (<=) (FreeVar _ _) (BoundedVar _) = False
    (<=) (FreeVar _ _) _ = True

    (<=) (UFunc _ _) (UFunc _ _) = True
    (<=) (UFunc _ _) (BFunc _ _ _) = True
    (<=) (UFunc _ _) (FFunc _ _) = True
    (<=) (UFunc _ _) _ = False
    (<=) (BFunc _ _ _) (BFunc _ _ _) = True
    (<=) (BFunc _ _ _) (FFunc _ _) = True
    (<=) (BFunc _ _ _) _ = False
    (<=) (FFunc _ _) (FFunc _ _) = True
    (<=) (FFunc _ _) _ = False

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
    debug (BFunc (Infix c) a b) = let (_, n, _, _) = bFuncFromConstr (Infix c)
                                  in "(" <> debug a <> " " <> unpack n <> " " <> debug b <>")"
    debug (BFunc (Prefix c) a b) = "(" <> debug c <> " " <> debug a <> " " <> debug b <> ")"
    debug (BoundedVar x) = "(Var " <> unpack x <> ")"
    debug (FreeVar v ex) = "(Var " <> unpack v <> "=" <> debug ex <> ")"

instance Show n => Debug (Function n) where
    debug (Function name params expr) = unpack name <> "(" <> intercalate "," (map unpack params) <> ") = " <> debug expr

instance Listable UnaryFunction where
    list = [Sin, Cos, Tan, USub, Ceiling, Floor, Sqrt, Succ, Pred, Abs, Asin, Acos, Atan, Sinh, Cosh, Tanh, Asinh, Acosh, Atanh]

instance Listable BinaryFunction where
    list = {-map Prefix [Log, Max, Min] ++-} map Infix [Add, BSub, Mult, Div, Expo]

instance Listable Constant where
    list = [E, Pi]

instance RealFloat n => Listable (Expr n) where
    tiers = cons1 Z
         \/ [map R [0, 1, -1, 2*pi, -0.000001, 0.000002, 1213123123, -54764235]]
         \/ cons2 UFunc
         \/ cons3 BFunc
         \/ cons1 Const

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
eval (UFunc c a)    = let (_, name, f, domain) = uFuncFromConstr c 
                      in case (eval a, domain) of
                         (Left er, _) -> Left er
                         (Right x, Just d) -> maybe (Right (f x)) Left (d x)
                         (Right x, Nothing) -> Right (f x)
eval (BFunc c a b)  = let (_, name, f, domain) = bFuncFromConstr c 
                      in case (eval a, eval b, domain) of
                            (Right a', Right b', Nothing) -> Right (f a' b')
                            (Right a', Right b', Just  d) -> maybe (Right (f a' b')) Left (d a' b')
                            (Left err, _, _) -> Left err
                            (_, Left err, _) -> Left err
eval (FFunc f@(Function name params expr) args) = eval (betaReduce expr (zip params args))


inline :: Expr n -> Expr n
inline (FreeVar v ex) = ex
inline (FFunc f@(Function _ params expr) args) = inline (betaReduce expr (zip params args))
inline (UFunc c a) = UFunc c (inline a)
inline (BFunc c a b) = BFunc c (inline a) (inline b)
inline ex = ex

