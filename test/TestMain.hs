
import TestParser (testParser)
import TestExpr (testExpr)
import TestCalculus (propSimplify)

import Test.LeanCheck
import Test.HUnit


main :: IO Counts
main = checkFor 10000 propSimplify >> runTestTT (TestList [testParser, testExpr])
