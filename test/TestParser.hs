
{-# LANGUAGE OverloadedStrings #-}

module TestParser where

import Test.HUnit ( assertEqual, Test(TestCase, TestList) )
import Data.Either (fromRight)
import Data.Number.CReal


import ParseExpr
import Expr

testParser :: Test
testParser = TestList [twoPlusThreePlusFour, parenPrecedence, multPrecedence, noMult]

twoPlusThreePlusFour, parenPrecedence, multPrecedence :: Test
twoPlusThreePlusFour = TestCase (assertEqual "2+3+4 -> (2+3)+4" (Right (BFunc (Infix Add) (BFunc (Infix Add) (Z 2 :: Expr CReal) (Z 3)) (Z 4))) (parse parseExpr [] "2+3+4"))
parenPrecedence = TestCase (assertEqual "(2+3)*4" (Right (BFunc (Infix Mult) (BFunc (Infix Add) (Z 2 :: Expr CReal) (Z 3)) (Z 4))) (parse parseExpr [] "(2+3)*4"))
multPrecedence = TestCase (assertEqual "2+3*4" (Right (BFunc (Infix Add) (Z 2 :: Expr CReal) (BFunc (Infix Mult) (Z 3) (Z 4)))) (parse parseExpr [] "2+3*4"))


noMult :: Test
noMult = TestCase (assertEqual "2 - (3)" (Right (BFunc (Infix BSub) (Z 2 :: Expr CReal) (Z 3))) (parse parseExpr [] "2 - (3)"))
