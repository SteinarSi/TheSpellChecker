
import TestParser (testParser, propParseIsOppositeOfShow)
import TestExpr (testExpr)
import TestCalculus (propSimplify)

import Test.LeanCheck
import Test.HUnit


main :: IO Counts
main = do
    mapM_ print (counterExamples 1000 propParseIsOppositeOfShow)
    mapM_ print (counterExamples 1000 propSimplify)
    runTestTT (TestList [testParser, testExpr]) 