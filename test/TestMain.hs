
import TestParser (testParser, propParseIsOppositeOfShow)
import TestExpr (testExpr)
import TestCalculus (propSimplify)

import Test.LeanCheck
import Test.HUnit


import Debug.Trace


main :: IO Counts
main = do
    mapM_ print (counterExamples 100000 propParseIsOppositeOfShow)
    --mapM_ print (counterExamples 100000 propSimplify)
    runTestTT (TestList [trace "a" testParser, trace "b" testExpr]) 