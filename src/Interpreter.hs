{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import qualified Data.Text as T


import Data.Either (either)
import Data.Number.CReal
import TextShow (showb)
import Data.List (delete)

import Expr
import ParseExpr
import ParseREPL



loop :: [Function CReal] -> IO ()
loop fs = do
    inn <- getLine
    case parse parseCommand fs (T.pack inn) of 
        Left err -> print err >> loop fs
        Right ex -> case ex of
            Help -> readFile "data/help.txt" >>= putStrLn >> loop fs
            Quit -> putStrLn "Bye-bye!"
            NewFunction f -> putStrLn ("I parsed the function like this: " ++ show f) >> loop (f : delete f fs)
            EvalFunction f args -> either (putStrLn . T.unpack) (\n -> putStrLn (inn ++ " = " ++ show n)) (evalFunction f args) >> loop fs
            EvalConstant ex -> putStrLn (show ex <> " = " <> show (evalConstant ex)) >> loop fs
            ShowFunction f -> print f >> loop fs
