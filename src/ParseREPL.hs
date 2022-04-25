
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}


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
import Data.Either (fromRight)

import Expr
import ParserUtility ( parse, failT, letter, many1, tryWhatever, Parser, ParseError, UserData(..), parseRealFloat)
import ParseExpr ( parseExpr )
import Calculus (differentiate)
import Utility (applyNtimesM)
import Draw


data Command n = NewFunction (Function n)
               | NewVariable Text (Expr n)
               | ShowFunction (Function n)
               | Eval (Expr n)
               | NoAction
               | Draw
               | Clear
               | AddDrawable (Drawable n)
               | Quit
               | Help

parseCommand :: (Show n, RealFloat n) => [UserData n] -> Text -> Either ParseError (Command n)
parseCommand = parse parseCommand'
    where 
        parseCommand' :: (RealFloat n, Show n) => Parser n (Command n)
        parseCommand' = try (Help <$ (string "help" <|> string "Help" <|> string "h") <* eof)   
                <|> try (Quit <$ (string "quit" <|> string "q" <|> string "Quit") <* eof)
                <|> try (Clear <$ (string "clear" <|> string "c" <|> string "Clear") <* eof)
                <|> try (Draw <$ (string "draw" <|> string "d" <|> string "Draw") <* eof)
                <|> try (ShowFunction <$> parseFunctionName <* eof)
                <|> try (Eval <$> parseExpr <* eof)
                <|> try (NewFunction <$> (Function <$> parseName <*> (char '(' *> parseParams <* char ')' <* char '=') <*> parseExpr <* eof))
                <|> try (NewVariable . T.pack <$> many1 letter <*> (many (char ' ') *> char '=' *> parseExpr ))
                <|> try (AddDrawable <$> (string "add" *> (parsePoint <|> (DFunction <$> parseFunctionName))))
                <|> NoAction <$ eof

parseFunctionName :: (RealFloat n) => Parser n (Function n)
parseFunctionName = do
    name <- T.pack <$> many1 letter
    d <- length <$> many (char '\'')
    (fs, _) <- lift get
    case listToMaybe [ f | UserFunction f@(Function fname params ex) <- fs, fname == name ] of
        Nothing -> fail "No such function has been defined"
        Just f@(Function fname [] ex) -> pure f
        Just f@(Function fname (p:ps) ex) -> case applyNtimesM d (`differentiate` p) ex of
            Left err -> failT err
            Right ex -> pure $ Function (fname <> T.pack (replicate d '\'')) (p:ps) ex

parsePoint :: (Show n, RealFloat n) => Parser n (Drawable n)
parsePoint = Point <$> (char '(' *> parseConstant <* char ',') <*> parseConstant <* char ')'

parseName :: Parser n Text
parseName = T.pack <$> many letter

parseParams :: (RealFloat n) => Parser n [Parameter]
parseParams = (do
        p <- T.pack <$> many1 letter
        (_, ps) <- lift get
        if elem p ps then failT ("Repeat instance of parameter " <> p)
        else lift (modify (fmap (p:))) *> fmap (p :) parseParams
  ) <|> char ',' *> parseParams
    <|> pure []

parseConstant :: (RealFloat n, Show n) => Parser n n
parseConstant = do
    ex <- parseExpr
    case eval ex of
        Left _ -> fail "bruh"
        Right n -> pure n
