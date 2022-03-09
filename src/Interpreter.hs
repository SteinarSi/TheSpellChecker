{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import qualified Data.Text as T


import Data.Either (either)
import Data.Number.CReal
import TextShow (showb)
import Data.List (delete)

import Expr
import ParseExpr ( parse )
import ParseREPL( parseCommand,Command(..) )
import Calculus (simplify)



loop :: [Function CReal] -> IO ()
loop fs = do
    inn <- getLine
    case parse parseCommand fs (T.pack inn) of 
        Left err -> print err >> loop fs
        Right ex -> case ex of
            Help -> readFile "data/help.txt" >>= putStrLn >> loop fs
            Quit -> putStrLn "Bye-bye!"
            NewFunction f@(Function name params ex) -> do
                putStrLn ("I parsed the function like this: " ++ show f)
                putStrLn ("And it was simplified to this: " ++ show (simplify ex))
                loop (Function name params (simplify ex) : delete f fs)
            EvalFunction f args -> either (putStrLn . T.unpack) (\n -> putStrLn (inn ++ " = " ++ show n)) (evalFunction f args) >> loop fs
            EvalConstant ex -> putStrLn (show ex <> " = " <> show (evalConstant ex)) >> loop fs
            ShowFunction f -> print f >> loop fs
