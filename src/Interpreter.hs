{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import qualified Data.Text as T


import Data.Either (either)
import Data.Number.CReal
import TextShow (showb, toString)
import Data.List (delete)

import Expr
import ParseExpr ( parse )
import ParseREPL( parseCommand,Command(..) )
import Calculus (simplify)



-- TODO, gjør om til REPL
loop :: [Function Double] -> IO ()
loop fs = do
    inn <- getLine
    case parse parseCommand fs (T.pack inn) of 
        Left err -> print err >> loop fs
        Right ex -> case ex of
            Help -> readFile "data/help.txt" >>= putStrLn >> loop fs
            Quit -> putStrLn "Bye-bye!"
            NewFunction f@(Function name params ex) -> do
                putStrLn ("I parsed the function like this: " ++ debug f)
                putStrLn ("And it was simplified to this: " ++ debug (simplify ex))
                loop (Function name params (simplify ex) : delete f fs)
            EvalConstant ex -> putStrLn (toString (showb ex <> " = " <> showb (evalConstant ex))) >> loop fs
            ShowFunction f -> print f >> loop fs
