
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
import ParserUtility
import ParseExpr
import ParserUtility (tryWhatever)



data Command n = NewFunction (Function n)
               | EvalFunction (Function n) [Argument n]
               | ShowFunction (Function n)
               | Quit
               | Help

parseCommand :: (RealFloat n, Show n, TextShow n) => Parser n (Command n)
parseCommand = try (Help <$ (string "help" <|> string "Help"))
           <|> try (Quit <$ (string "quit" <|> string "q" <|> string "Quit"))
           <|> try (NewFunction <$> (Function <$> parseName <*> (char '(' *> parseParams <* char ')' <* char '=') <*> parseExpr))
           <|> try (ShowFunction <$> parseFunctionName <* eof)
           <|> (do
               f@(Function fname params expr) <- parseFunctionName
               EvalFunction f <$> (char '(' *> parseArgs params <* char ')'))

parseConstant :: (RealFloat n, Show n, TextShow n) => Parser n n
parseConstant = do
    e <- parseExpr 
    if isConstant e then pure (evalConstant e)
    else fail "Function argument has to be a constant expression"

parseFunctionName :: Parser n (Function n)
parseFunctionName = do
    name <- T.pack <$> many1 letter
    (fs, _) <- lift get
    maybe (fail "No such function has been defined") pure (listToMaybe [ f | f@(Function fname _ _) <- fs, fname == name ])

parseName :: Parser n Text
parseName = T.pack <$> many letter

parseArgs :: (RealFloat n, Show n, TextShow n) => [Parameter] -> Parser n [Argument n]
parseArgs [] = pure []
parseArgs (p:ps) = (:) . (,) p <$> parseConstant <*> tryWhatever (char ',') (parseArgs ps)

parseParams :: (RealFloat n, Show n, TextShow n) => Parser n [Parameter]
parseParams = (do
        p <- T.pack <$> many1 letter
        (_, ps) <- lift get
        if elem p ps then failT ("Repeat instance of parameter " <> p)
        else lift (modify (fmap (p:))) *> fmap (p :) parseParams
  ) <|> char ',' *> parseParams
    <|> pure []

