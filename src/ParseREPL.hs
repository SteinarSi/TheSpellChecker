
{-# LANGUAGE OverloadedStrings #-}


module ParseREPL where

import Data.Number.RealCyclotomic
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec ((<|>), try, eof, many)
import Text.Megaparsec.Char (char, string)
import Control.Monad.State.Lazy (get, put, modify)
import Control.Monad.Trans (lift)
import Data.Maybe (listToMaybe, maybe)

import Expr
import ParserUtility
import ParseExpr


data Command = NewFunction Function
             | EvalFunction Function [Argument]
             | Help

parseCommand :: Parser Command
parseCommand = try (Help <$ (string "help" <|> string "Help"))
           <|> try (NewFunction <$> (Function <$> parseName <*> (char '(' *> parseParams <* char ')' <* char '=') <*> parseExpr))
           <|> (do
               name <- parseName
               (fs, _) <- lift get
               f@(Function fname params expr) <- maybe (fail "No such function has been defined") pure (listToMaybe [ f | f@(Function fname _ _) <- fs, fname == name ])
               char '('
               a <- many (tryWhatever (char ',') parseConstant)
               char ')'
               eof
               pure (EvalFunction f (zip params a)))

    where args = fmap (:) parseConstant  <*>  (char ',' *> args ) <|> pure []

parseConstant :: Parser RealCyclotomic 
parseConstant = do
    e <- parseExpr 
    if isConstant e then pure (evalConstant e)
    else fail "Function argument has to be a constant expression"

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
