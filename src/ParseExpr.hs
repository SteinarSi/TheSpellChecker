{-# LANGUAGE OverloadedStrings #-}


module ParseExpr (parseExpr, parse) where

import Text.Megaparsec ( (<|>), many, oneOf, MonadParsec(try, parseError), ParsecT, runParserT, ParseErrorBundle )
import Text.Megaparsec.Char (char, string)
import Data.Void (Void)
import qualified Data.Text as T
import Data.Text (Text)
import TextShow (TextShow)
import Text.Read (readMaybe)
import Data.Functor (($>))
import Data.Function ((&))
import Data.List (foldl', find)
import Data.Number.RealCyclotomic (RealCyclotomic)

import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State (State, get, runState, evalState, modify)

import Expr
import Utility
import ParserUtility

{-

Function     => Name (Params) = Expr
Name         => Letter | Letter Var
Params       => Param | , Param | ε
Param        => Letter | Letter Var
Expr         => Term Expr' | - Expr
Expr'        => + Term Expr' | - Term Expr' | ε
Term         => Factor Term'
Term'        => * Factor Term' | / Factor Term' | Factor Term' | ε
Factor       => Num ^ Factor | Num
Num          => (Expr) | Var | e | pi | RealCyclotomic | FunctionCall
FunctionCall => Log[Expr](Expr) | Log RealCyclotomic (Expr) | Log (Expr) | ln(Num) | ...... etc, legg til flere her senere
Var          => Letter | Letter Var
Letter       => a | b | c | .... | z | A | B | ... | Z | α | β | ... | ω      (unntatt π)

-}





parseExpr :: (RealFloat n) => Parser n (Expr n)
parseExpr = UFunc USub <$> (char '-' *> parseExpr)
        <|> foldl' (&) <$> parseTerm <*> parseTermList

parseTerm :: (RealFloat n) => Parser n (Expr n)
parseTerm = foldl' (&) <$> parseFactor <*> parseFactorList

-- Lager en liste på formen [ +term, -term, -term, +term ], som kan settes sammen venstreassosiativt med en initiell term.
parseTermList :: (RealFloat n) => Parser n [Expr n -> Expr n]
parseTermList = fmap ((:) . flip (BFunc (Infix Add)))  (char '+' *> parseTerm) <*> parseTermList
            <|> fmap ((:) . flip (BFunc (Infix BSub))) (char '-' *> parseTerm) <*> parseTermList
            <|> pure []

parseFactor :: (RealFloat n) => Parser n (Expr n)
parseFactor = parseNum >>= \n -> BFunc (Infix Expo) n <$> (char '^' *> parseFactor) <|> pure n

-- Lager en liste på formen [ *faktor, /faktor, *faktor ], som kan settes sammen venstreassosiativt med en initiell faktor
-- Dette tillater også å droppe gangetegnet i uttrykk, f. eks. 2(3+4) = 2*(3+4).
parseFactorList :: (RealFloat n) => Parser n [Expr n -> Expr n]
parseFactorList = fmap ((:) . flip (BFunc (Infix Mult))) ((char '*' *> parseFactor) <|> parseFactor) <*> parseFactorList
              <|> fmap ((:) . flip (BFunc (Infix Div )))  (char '/' *> parseFactor) <*> parseFactorList
              <|> pure []

parseVar :: Parser n (Expr n)
parseVar = do
    var <- T.pack <$> many1 letter
    (dt, ps) <- lift get -- look at me, I understand monad transformers now!
    if var `elem` ps then pure (BoundedVar var)
    else case find (==UserVariable var (Z 0)) dt of
         Nothing -> failT ("Unrecognized variable name: " <> var)
         Just (UserVariable v ex) -> pure (FreeVar v ex)
         Just _ -> error "bruh"
    
    --maybe (failT ("Unrecognized variable name: " <> var)) (\(UserVariable n ex) -> FreeVar n ex) (find (==UserVariable var (Z 0)) dt)

parseParam :: Parser n String
parseParam = many1 letter

parseNum :: (RealFloat n) => Parser n (Expr n)
parseNum = try (R <$> parseFloat)
       <|> try (Z <$> parseInteger)
       <|> try (char '(' *> parseExpr <* char ')')
       <|> try parseVar
       <|> char 'e' $> Const E
       <|> try (string "pi" <|> string "Pi" <|> string "π") $> Const Pi
       <|> parseFunctionCall

parseFunctionCall :: (RealFloat n) => Parser n (Expr n)
parseFunctionCall = try (BFunc (Prefix Log) (Const E) <$> ((string "log" <|> string "Log" <|> string "ln" <|> string "Ln") *> char '(' *> parseExpr <* char ')')) --log med e som base
       <|> try (BFunc (Prefix Log) <$> ((string "log" <|> string "Log") *> (char '[' *> parseExpr <* char ']' <|> fmap Z parseInteger)) <*> (char '(' *> parseExpr <* char ')')) -- log med custom base
       <|> try (do
            name <- first [ try (string fname) | (_, fname, _) <- uFunctions, fname /= "-"]
            let Just (c, _, _) = uFuncFromName name
            UFunc c <$> (char '(' *> parseExpr <* char ')') )
       <|> try (do
            name <- first [ try (string fname) | (_, fname, _) <- bFunctions]
            let Just (c, _, _) = bFuncFromName name
            BFunc c <$> (char '(' *> parseExpr <* char ',') <*> parseExpr <* char ')' )
       <|> (do
            name <- T.pack <$> many1 letter
            (fs, _) <- lift get
            case [ f | UserFunction f@(Function fname _ _)<-fs, name == fname ] of
                [] -> failT ("Unrecognized function: " <> name)
                (f@(Function fname params ex):_) -> FFunc f <$> (char '(' *> replicateM (length params) (tryWhatever (char ',') parseExpr) <* char ')')
       )