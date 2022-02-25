
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack, unpack)
import TextShow (toString, showb)

import Data.IORef
import Expr 
import Parser


main :: IO ()
main = loop []


loop :: [Function] -> IO ()
loop fs = do
    inn <- getLine
    case parseFunction fs (pack inn) of
        Left er -> print er >> loop fs
        Right f -> putStrLn ("Function parsed like this: " ++ show f) >> loop (f:fs)


