
import TestParser (testParser)
import TestExpr (testExpr)
import TestCalculus (propSimplify)

import Test.LeanCheck
import Test.HUnit


main :: IO Counts
main = mapM_ print (counterExamples 100000 propSimplify) >> runTestTT (TestList [testParser, testExpr]) 