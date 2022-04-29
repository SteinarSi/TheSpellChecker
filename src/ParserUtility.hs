

{-# LANGUAGE OverloadedStrings #-}

module ParserUtility where


import Text.Megaparsec (ParsecT, runParserT, many, oneOf, (<|>), try)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Megaparsec.Char (string)
import Control.Monad.State.Lazy
import Data.Void (Void)
import Data.Text (Text)
import Data.Text.Read (double)
import Data.List (foldl', find, intercalate)
import qualified Data.Text as T

import Expr
import Utility




type Parser n = ParsecT Void Text (State ([UserData n], [Text]))
type ParseError = ParseErrorBundle Text Void
data UserData n = UserFunction (Function n)
                | UserVariable Text (Expr n)

instance (RealFloat n, Show n) => Show (UserData n) where
    show (UserVariable name ex) = T.unpack name <> " = " <> show ex
    show (UserFunction (Function name params ex)) = T.unpack name <> "(" <> intercalate "," (map T.unpack params) <> ") = " <> show ex

instance Eq (UserData n) where
    (==) (UserFunction (Function name1 _ _)) (UserFunction (Function name2 _ _)) = name1 == name2
    (==) (UserVariable name1 _) (UserVariable name2 _) = name1 == name2
    (==) _ _ = False


parse :: Parser n a -> [UserData n] -> Text -> Either ParseError a
parse prsr dt text = evalState (runParserT prsr "User input" (T.filter (/=' ') text)) (dt, [])

parseRealFloat :: RealFloat n => Parser n n
parseRealFloat = do
    before <- withDefault (T.pack <$> many digit) "0"
    comma  <- withDefault (string ".") ""
    after  <- withDefault (T.pack <$> many digit) "0"
    case double (before <> comma <> after) of
        Left err     -> fail err
        Right (n, _) -> pure (realToRat n)

parseFloat :: RealFloat n => Parser n n
parseFloat = do
    before <- withDefault (T.pack <$> many digit) "0"
    comma  <- string "."
    after  <- withDefault (T.pack <$> many digit) "0"
    case double (before <> comma <> after) of
        Left err     -> fail err
        Right (n, _) -> pure (realToRat n)

parseVar :: Parser n (Expr n)
parseVar = do
    var <- T.pack <$> many1 letter
    (dt, ps) <- lift get -- look at me, I understand monad transformers now!
    if var `elem` ps then pure (BoundedVar var)
    else case find (==UserVariable var (Z 0)) dt of
         Nothing -> failT ("Unrecognized variable name: " <> var)
         Just (UserVariable v ex) -> pure (FreeVar v ex)
         Just _ -> error "bruh"

parseInteger :: Parser n Integer
parseInteger = read <$> many1 digit

digit :: Parser n Char
digit = oneOf ['0'..'9']

withDefault :: Parser n a -> a -> Parser n a
withDefault rule default' = try rule <|> pure default'

-- Gjør den første regelen, og bryr seg ikke om den feiler eller ikke før den tar den neste regelen.
tryWhatever :: Parser n a -> Parser n b -> Parser n b
tryWhatever ignore rule = (try ignore *> rule) <|> rule

letter :: Parser n Char
letter = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ', 'ι', 'κ', 'λ', 'μ', 'ν', 'ξ', 'ρ', 'σ', 'ς', 'τ', 'υ', 'φ', 'χ', 'ψ', 'ω', 'א'])

many1 :: Parser n a -> Parser n [a]
many1 a = (:) <$> a <*> many a

first :: [Parser n a] -> Parser n a
first = foldl' (<|>) (fail "No rule applied")


failT :: MonadFail m => Text -> m a
failT = fail . T.unpack

