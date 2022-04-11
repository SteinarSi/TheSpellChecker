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
import ParserUtility (UserData(..))
import Calculus (simplify, simplifyFunction)



loop :: [UserData Double] -> IO ()
loop fs = do
    inn <- getLine
    case parseCommand fs (T.pack inn) of 
        Left err -> print err >> loop fs
        Right ex -> case ex of
            Help -> readFile "data/help.txt" >>= putStrLn >> loop fs
            Quit -> putStrLn "Bye-bye!"
            NoAction -> loop fs
            NewFunction f@(Function name params ex) -> let uf = UserFunction (simplifyFunction f) in loop (uf : delete uf fs)
            NewVariable name ex -> loop (UserVariable name (simplify ex) : fs)
            Eval ex -> putStrLn (either T.unpack (\r -> toString (showb ex <> " = " <> showb r)) (eval ex)) >> loop fs
            ShowFunction f -> print f >> loop fs
