{-# LANGUAGE OverloadedStrings #-}


module Parser (parseTester) where

import Text.Megaparsec ( (<|>), many, oneOf, MonadParsec(try, parseError), ParsecT, runParserT, ParseErrorBundle )
import Text.Megaparsec.Char (char, string)
import Data.Void (Void)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Read (double)
import Text.Read (readMaybe)
import Data.Functor (($>))
import Data.Function ((&))
import Data.List (foldl')
import Data.Number.RealCyclotomic (RealCyclotomic)

import Prelude hiding (pi)
import qualified Prelude as P

import Control.Monad.Trans.Class (lift)
import Control.Monad.State (State, get, runState, evalState, modify)

import Expr (Expr(..), Function(Function), Parameter, realToRat)

{-

Function     => Name (Params) = Expr
Name         => Letter | Letter Var
Params       => Param | , Param | ε
Param        => Letter | Letter Var
Expr         => Term Expr' | - Expr
Expr'        => + Term Expr' | - Term Expr' | ε
Term         => Factor Term'
Term'        => * Factor Term' | / Factor Term' | ε
Factor       => Num ^ Factor | Num
Num          => (Expr) | Var | e | pi | RealCyclotomic | FunctionCall
FunctionCall => Log[Expr](Expr) | Log RealCyclotomic (Expr) | Log (Expr) | ln(Num) | ...... etc, legg til flere her senere
Var          => Letter | Letter Var
Letter       => a | b | c | .... | z | A | B | ... | Z | α | β | ... | ω      (unntatt π)

-}

type Parser = ParsecT Void Text (State ([Function], [Text]))

parseTester :: Text -> Either (ParseErrorBundle Text Void) Function
parseTester s = evalState (runParserT parseFunction "Test" (T.filter (/=' ') s)) ([], [])


parseFunction :: Parser Function
parseFunction = Function <$> parseName <*> (char '(' *> parseParams <* char ')' <* char '=') <*> parseExpr

parseName :: Parser Text 
parseName = T.pack <$> many1 letter

parseParams :: Parser [Parameter]
parseParams = (do
        p <- T.pack <$> many1 letter
        (_, ps) <- lift get
        if elem p ps then failT ("Repeat instance of parameter " <> p)
        else lift (modify (fmap (p:))) *> fmap (p :) parseParams
  ) <|> char ',' *> parseParams
    <|> pure []

parseExpr :: Parser Expr
parseExpr = USub <$> (char '-' *> parseExpr)
        <|> foldl' (&) <$> parseTerm <*> parseTermList

parseTerm :: Parser Expr
parseTerm = foldl' (&) <$> parseFactor <*> parseFactorList

-- Lager en liste på formen [ +term, -term, -term, +term ], som kan settes sammen venstreassosiativt med en initiell term.
parseTermList :: Parser [Expr -> Expr]
parseTermList = fmap ((:) . flip Add)  (char '+' *> parseTerm) <*> parseTermList
            <|> fmap ((:) . flip BSub) (char '-' *> parseTerm) <*> parseTermList
            <|> pure []

parseFactor :: Parser Expr
parseFactor = parseNum >>= \n -> Expo n <$> (char '^' *> parseFactor) <|> pure n

-- Lager en liste på formen [ *faktor, /faktor, *faktor ], som kan settes sammen venstreassosiativt med en initiell faktor
parseFactorList :: Parser [Expr -> Expr]
parseFactorList = fmap ((:) . flip Mult) (char '*' *> parseFactor) <*> parseFactorList
              <|> fmap ((:) . flip Div)  (char '/' *> parseFactor) <*> parseFactorList
              <|> pure []

parseVar :: Parser Expr
parseVar = do
    var <- T.pack <$> many1 letter
    (_, ps) <- lift get -- look at me, I understand monad transformers now!
    if var `elem` ps then pure (Var var)
    else failT ("Unrecognized variable name: " <> var)

parseParam :: Parser String
parseParam = many1 letter

letter :: Parser Char
letter = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ', 'ι', 'κ', 'λ', 'μ', 'ν', 'ξ', 'ρ', 'σ', 'ς', 'τ', 'υ', 'φ', 'χ', 'ψ', 'ω', 'א'])

parseNum :: Parser Expr
parseNum = try (Num <$> parseCyclotomic)
       <|> try (char '(' *> parseExpr <* char ')')
       <|> try parseVar
       <|> char 'e' $> Num e
       <|> (string "pi" <|> string "Pi" <|> string "π") $> Num pi
       <|> parseFunctionCall

parseFunctionCall :: Parser Expr
parseFunctionCall = try (Log (Num e) <$> ((string "log" <|> string "Log" <|> string "ln" <|> string "Ln") *> char '(' *> parseExpr <* char ')')) --log med e som base
       <|> try (Log <$> ((string "log" <|> string "Log") *> (char '[' *> parseExpr <* char ']' <|> fmap Num parseCyclotomic)) <*> (char '(' *> parseExpr <* char ')')) -- log med custom base


parseCyclotomic :: Parser RealCyclotomic
parseCyclotomic = do
    before <- withDefault (T.pack <$> many digit) "0"
    comma  <- withDefault (string ".") ""
    after  <- withDefault (T.pack <$> many digit) "0"
    case double (before <> comma <> after) of
        Left err      -> fail err
        Right  (n, _) -> pure (realToRat n)

digit :: Parser Char
digit = oneOf ['0'..'9']

withDefault :: Parser a -> a -> Parser a
withDefault rule default' = try rule <|> pure default'

many1 :: Parser a -> Parser [a]
many1 a = (:) <$> a <*> many a

e :: RealCyclotomic
e = 2.71828182845904523536028747

pi :: RealCyclotomic
pi = realToFrac P.pi

failT :: MonadFail m => Text -> m a
failT = fail . T.unpack