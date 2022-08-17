module B10.Test
    (
        testB10Value
    ,   testB10RandomConversion
    ,   testBxValue
    ,   testSomeFixedB10Values
    ,   testSomeFixedBxValues
    ,   testSomeBoundaryConditions
    ,   runSomeTests
    )   where

import Debug.Trace                              (traceShow, traceShowId)
import GHC.Integer.GMP.Internals                (oneBigNat)
import Data.Maybe                               (fromJust, Maybe) 
import Data.List                                (elemIndex, sort)
import B10.Convert

testB10Value :: Integer -> Bool
testB10Value b10Value = do
    case convertBxToB10 (convertB10ToBx b10Value) of
        Left convertedB10Value -> convertedB10Value == b10Value
        Right err -> traceShow (show err) False

testBxValue :: String -> Bool 
testBxValue bxValue = do
    case convertBxToB10 bxValue of
        Left convertedB10Value -> bxValue == convertB10ToBx convertedB10Value
        Right err -> traceShow (show err) False

-- these fixed tests are valid for b56
testSomeFixedB10Values :: String
testSomeFixedB10Values = do
    let b10Tests = filter (==False) [testB10Value b10 | b10 <- [floor(pi*1000000000000), 9522230638, 602214076 * 10^15]]
    if null b10Tests then "Pass"
    else "Fail"

testSomeFixedBxValues :: String
testSomeFixedBxValues = do
    let bxTests  = and [testBxValue bx | bx <- ["1f209b6e", "1F209B6E", "0", concat (replicate 30 "a")]]
    if bxTests then "Pass" 
    else "Fail"

testSomeBoundaryConditions :: String 
testSomeBoundaryConditions = do
    let bxTests = and [testBxValue bx | bx <-  ["A", "z", "10", "zz", "200", "0", concat(replicate 80 "z")]]
        b10Tests = and [testB10Value b10 | b10 <- [0, 55, 56, 112, 167, 168, 3080, 3081, 172480, 10^99]]
    if bxTests && b10Tests then "Pass"
    else traceShow (show (bxTests, b10Tests)) "Fail"

testB10RandomConversion :: Integer -> String
testB10RandomConversion b10Value = do
    {- 
      This 'sort of' randomizes a test by simply sorting the bx values after conversion from b10.  
      which usually yields a different b10 value (for larger numbers) which is then converted 
      both directions and compared.  Just a toy. 
    -}
    let bxValue = convertB10ToBx b10Value
        randomBxValue = sort bxValue
        paddedPlaceholderCount = length $ takeWhile (== head ([fst pair | pair <- buildBase, snd pair == 0])) randomBxValue
        randomBxValueCorrected = drop paddedPlaceholderCount randomBxValue
    case convertBxToB10 randomBxValueCorrected of
        Left firstConversion -> do
            let convertedValue = convertB10ToBx firstConversion
            if randomBxValueCorrected == convertedValue then traceShow (show bxValue, randomBxValue, randomBxValueCorrected, convertedValue) "Pass"
            else traceShow (show bxValue, randomBxValue, randomBxValueCorrected, convertedValue) "Fail"
        Right err -> traceShow (show err) "Fail"

runSomeTests :: String
runSomeTests = do
    let {severalTests = filter failureTest [testSomeFixedBxValues, testSomeFixedB10Values, testSomeBoundaryConditions]
        where failureTest = (== "Fail")} -- I'm not entirely sure why this set of brackets is necessary...
    if null severalTests then do "Pass"
    else traceShow (show severalTests) "Fail"