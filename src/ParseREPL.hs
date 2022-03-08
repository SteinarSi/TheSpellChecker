
{-# LANGUAGE OverloadedStrings #-}


module ParseREPL where

import Data.Number.RealCyclotomic
import Data.Text (Text)
import qualified Data.Text as T
import TextShow (TextShow)
import Text.Megaparsec ((<|>), try, eof, many)
import Text.Megaparsec.Char (char, string)
import Control.Monad.State.Lazy (get, put, modify)
import Control.Monad.Trans (lift)
import Data.Maybe (listToMaybe, maybe)

import Expr
import ParserUtility ( failT, letter, many1, tryWhatever, Parser )
import ParseExpr ( parseExpr )
import Calculus (differentiate)
import Utility (applyNtimesM)



data Command n = NewFunction (Function n)
               | EvalFunction (Function n) [Argument n]
               | EvalConstant (Expr n)
               | ShowFunction (Function n)
               | Quit
               | Help

parseCommand :: (RealFloat n, Show n, TextShow n) => Parser n (Command n)
parseCommand = try (Help <$ (string "help" <|> string "Help" <|> string "h") <* eof)   
           <|> try (Quit <$ (string "quit" <|> string "q" <|> string "Quit") <* eof)
           <|> try (do
               f@(Function fname params expr) <- parseFunctionName
               EvalFunction f <$> (char '(' *> parseArgs params <* char ')') <* eof)
           <|> try (ShowFunction <$> parseFunctionName <* eof)
           <|> try (NewFunction <$> (Function <$> parseName <*> (char '(' *> parseParams <* char ')' <* char '=') <*> parseExpr <* eof))
           <|> try (EvalConstant <$> parseConstant <* eof)

parseConstant :: (RealFloat n, Show n, TextShow n) => Parser n (Expr n)
parseConstant = do
    e <- parseExpr 
    if isConstant e then pure e
    else fail "Function argument has to be a constant expression"

parseFunctionName :: (RealFloat n, TextShow n, Show n) => Parser n (Function n)
parseFunctionName = do
    name <- T.pack <$> many1 letter
    d <- length <$> many (char '\'')
    (fs, _) <- lift get
    case listToMaybe [ f | f@(Function fname params ex) <- fs, fname == name ] of
        Nothing -> fail "No such function has been defined"
        Just f@(Function fname [] ex) -> pure f
        Just f@(Function fname (p:ps) ex) -> case applyNtimesM d (`differentiate` p) ex of
            Left err -> failT err
            Right ex -> pure $ Function fname (p:ps) ex

parseName :: Parser n Text
parseName = T.pack <$> many letter

parseArgs :: (RealFloat n, Show n, TextShow n) => [Parameter] -> Parser n [Argument n]
parseArgs [] = pure []
parseArgs (p:ps) = (:) . (,) p <$> fmap evalConstant parseConstant <*> tryWhatever (char ',') (parseArgs ps)

parseParams :: (RealFloat n, Show n, TextShow n) => Parser n [Parameter]
parseParams = (do
        p <- T.pack <$> many1 letter
        (_, ps) <- lift get
        if elem p ps then failT ("Repeat instance of parameter " <> p)
        else lift (modify (fmap (p:))) *> fmap (p :) parseParams
  ) <|> char ',' *> parseParams
    <|> pure []

