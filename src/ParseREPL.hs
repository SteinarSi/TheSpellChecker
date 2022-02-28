
{-# LANGUAGE OverloadedStrings #-}


module ParseREPL where

import Data.Number.RealCyclotomic
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec ((<|>), try)
import Text.Megaparsec.Char (char)
import Control.Monad.State.Lazy (get, put, modify)
import Control.Monad.Trans (lift)


import Expr
import ParserUtility
import ParseExpr


data Command = NewFunction Function
             | EvalFunction RealCyclotomic 
             | Help

--parseCommand :: Parser Command
--parseCommand = Help <$ (string "help" <|> string "Help")
--           <|> NewFunction <$ (many1 letter *> char '(' *> parseParams *> char ')')
--           <|> 

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