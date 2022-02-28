

{-# LANGUAGE OverloadedStrings #-}

module ParserUtility where


import Text.Megaparsec (ParsecT, runParserT, many, oneOf, (<|>), try)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Megaparsec.Char (string)
import Control.Monad.State.Lazy
import Data.Void (Void)
import Data.Text (Text)
import Data.Text.Read (double)
import qualified Data.Text as T
import Data.Number.RealCyclotomic

import Expr
import Utility




type Parser = ParsecT Void Text (State ([Function], [Text]))
type ParseError = ParseErrorBundle Text Void


parse :: Parser a -> [Function] -> Text -> Either ParseError a
parse prsr fs text = evalState (runParserT prsr "User input" (T.filter (/=' ') text)) (fs, [])

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

-- Gjør den første regelen, og bryr seg ikke om den feiler eller ikke før den tar den neste regelen.
tryWhatever :: Parser a -> Parser b -> Parser b
tryWhatever ignore rule = (try ignore *> rule) <|> rule

letter :: Parser Char
letter = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ', 'ι', 'κ', 'λ', 'μ', 'ν', 'ξ', 'ρ', 'σ', 'ς', 'τ', 'υ', 'φ', 'χ', 'ψ', 'ω', 'א'])

many1 :: Parser a -> Parser [a]
many1 a = (:) <$> a <*> many a

failT :: MonadFail m => Text -> m a
failT = fail . T.unpack