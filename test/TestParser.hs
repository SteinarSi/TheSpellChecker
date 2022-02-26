
{-# LANGUAGE OverloadedStrings #-}

module TestParser where

import Test.HUnit ( assertEqual, Test(TestCase, TestList) )
import Data.Either (fromRight)


import Parser (parseExpression)
import Expr ( Expr(..) )


testParser :: Test
testParser = TestList [twoPlusThreePlusFour, parenPrecedence, multPrecedence]

twoPlusThreePlusFour, parenPrecedence, multPrecedence :: Test
twoPlusThreePlusFour = TestCase (assertEqual "2+3+4 -> (2+3)+4" (Right (Add (Add (Num 2) (Num 3)) (Num 4))) (parseExpression "2+3+4"))
parenPrecedence = TestCase (assertEqual "(2+3)*4" (Right (Mult (Add (Num 2) (Num 3)) (Num 4))) (parseExpression "(2+3)*4"))
multPrecedence = TestCase (assertEqual "2+3*4" (Right (Add (Num 2) (Mult (Num 3) (Num 4)))) (parseExpression "2+3*4"))



