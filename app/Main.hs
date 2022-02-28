
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack, unpack)
import TextShow (toString, showb)

import Interpreter

main :: IO ()
main = loop []

