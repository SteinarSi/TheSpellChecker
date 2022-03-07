
{-# LANGUAGE OverloadedStrings #-}

module TestExpr where


import Test.HUnit ( Test(TestList, TestCase), assertEqual )
import TextShow (showb, toString)

import Expr
import ParseExpr
import GHC.Read (paren)


testExpr :: Test 
testExpr = TestList [noParensNecessary, parensNecessary]

noParensNecessary, parensNecessary :: Test
noParensNecessary = TestCase (assertEqual "2+3*4" "2+3*4" (toString (showb (fromRight (parse parseExpr [] "2+3*4")))))
parensNecessary = TestCase (assertEqual "(2+3)*4" "(2+3)*4" (toString (showb (fromRight (parse parseExpr [] "(2+3)*4")))))








fromRight :: Either a b -> b
fromRight (Left _) = undefined 
fromRight (Right b) = b