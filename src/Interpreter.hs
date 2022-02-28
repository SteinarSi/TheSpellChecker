{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import qualified Data.Text as T


import Expr
import ParseExpr
import ParseREPL


loop :: [Function] -> IO ()
loop fs = do
    inn <- getLine
    case parse parseFunction fs (T.pack inn) of
        Left er -> print er >> loop fs
        Right f -> putStrLn ("Function parsed like this: " ++ show f) >> loop (f:fs)