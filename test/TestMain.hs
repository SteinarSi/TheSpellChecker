
import TestParser (testParser)
import TestExpr (testExpr)

import Test.HUnit


main :: IO Counts
main = runTestTT (TestList [testParser, testExpr])
