{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import qualified Data.Text as T


import Data.Either (either)
import TextShow (showb, toString)
import Data.List (delete)

import Utility (debug)
import Expr ( Function(Function), eval, inline )
import ParseExpr ( parse )
import ParseREPL( parseCommand,Command(..) )
import ParserUtility (UserData(..))
import Calculus (simplify, simplifyFunction)
import qualified Draw as D



loop :: [UserData Double] -> [D.Drawable Double] -> IO ()
loop fs ds = do
    inn <- getLine
    case parseCommand fs (T.pack inn) of 
        Left err -> print err >> loop fs ds
        Right ex -> case ex of
            Help -> readFile "data/help.txt" >>= putStrLn >> loop fs ds
            Quit -> putStrLn "Bye-bye!"
            NoAction -> loop fs ds
            NewFunction f@(Function name params ex) -> let uf = UserFunction (simplifyFunction f) in loop (uf : delete uf fs) ds
            NewVariable name ex -> loop (UserVariable name (simplify ex) : fs) ds
            Eval ex -> putStrLn (either T.unpack (\r -> toString (showb ex <> " = " <> showb r)) (eval ex)) >> loop fs ds
            ShowFunction f -> print f >> loop fs ds
            Draw -> D.draw ds >> loop fs ds
            Clear -> loop fs []
            AddDrawable d -> loop fs (d:ds)
