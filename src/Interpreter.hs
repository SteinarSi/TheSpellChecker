{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import qualified Data.Text as T


import Expr
import ParseExpr
import ParseREPL


loop :: [Function] -> IO ()
loop fs = do
    inn <- getLine
    case parse parseCommand fs (T.pack inn) of 
        Left err -> print err >> loop fs
        Right ex -> case ex of
            Help -> putStrLn "There is no help to be found here yet"
            NewFunction f -> putStrLn ("I parsed the function like this: " ++ show f) >> loop (f:fs)
            EvalFunction f args -> case evalFunction f args of
                Left err -> print err
                Right  n -> putStrLn (inn ++ " = " ++ show n) >> loop fs
            
