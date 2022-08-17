module B10.Convert
    (
        convertB10ToBx
    ,   convertBxToB10
    ,   convertBxToB10'
    ,   buildBase
    )   where

import Debug.Trace                              (traceShow, traceShowId)
import GHC.Integer.GMP.Internals                (oneBigNat)
import Data.Maybe                               (fromJust, Maybe) 
import Data.List                                (elemIndex, sort)

buildBase :: [(Char, Integer)]
buildBase = do
    -- you can modify the base set by adjusting these two lines to exclude or include characters
    let removeChars = "OILoil"
        {-  
            It's not tested, but I did my best to abstract out edge cases like 0 to reference this list 
            (and its length) instead of a hard coded value.  In theory you should be able to modify
            removeChars and charSet and work with any size (or symbol) base character set.
        -}
        charSet = [bd | bd <- ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'], bd `notElem` removeChars]
        baseSize = length charSet
    zip charSet [0..fromIntegral (baseSize::Int)]

b10CalcUnitValue :: Char -> Integer -> Integer 
b10CalcUnitValue b56Digit b56Position = do
    let baseSize = fromIntegral (length buildBase)
        b10UnitValue = head ([snd pair | pair <- buildBase, fst pair == b56Digit])
    b10UnitValue * baseSize ^ b56Position 


convertB10ToBx:: Integer -> String
convertB10ToBx b10Value = do
    let (bx, _) = bxCalcUnitValue b10Value (bxLimit b10Value 0) [] 
    bx  

convertBxToB10 :: String -> Either Integer String 
convertBxToB10 bxValue = do
    if and [b `elem` [a | (a, d) <- buildBase ] | b <- bxValue] then Left $ do
        let b10 = sum [b10CalcUnitValue b10UnitVal (fromIntegral (pos::Int)) | (b10UnitVal, pos) <- zip (reverse bxValue) [0..length bxValue]]
        b10
    else Right $ do
        let err = "Cannot attempt to convert an invalid unit value in : '" ++ bxValue ++ "'. \n    The value is not in the base character set " ++ [a | (a,d) <- buildBase]
        err

convertBxToB10' :: String -> Integer
convertBxToB10' bxValue = case convertBxToB10 bxValue of
    Left  b10Value  -> b10Value
    Right err       -> (-1)

bxLimit :: Integer -> Integer -> Integer
bxLimit b10Value startValue = do
    let testValue = startValue + 1
        baseSize = fromIntegral (length buildBase)
    if b10Value < baseSize ^ testValue then startValue
    else bxLimit b10Value testValue

bxCalcUnitValue :: Integer -> Integer -> String -> (String, Integer)
bxCalcUnitValue b10Value b10UnitPosition bxPredicate = do
    let baseSize = fromIntegral (length buildBase)
        baseValues = [0..baseSize - 1] -- not strictly necessary, but might prevent a typo later
        -- calc all the lower bounds for this
        lowerBounds = takeWhile (<= b10Value) $ map (* baseSize ^ b10UnitPosition) baseValues
        lowerBound = maximum lowerBounds
        -- write once, read never...use the lowerBound to locate its position so we can retrieve the bx unit value
        lowerBoundPosition = fromIntegral $ fromJust $ elemIndex lowerBound $ map (* baseSize ^ b10UnitPosition) baseValues
        upperBound = lowerBoundPosition * baseSize ^ b10UnitPosition
        paddingValue = head ([fst pair | pair <- buildBase, snd pair == 0])
        bxUnitValue = head ([fst pair | pair <- buildBase, snd pair == lowerBoundPosition])
        b10Remainder = b10Value - upperBound
    if b10Remainder == 0 then do
        -- This might be a zero in a position that is not the far right position, so it needs padded with whatever our bx zero value is
        let newPredicate = calcPredicate(b10Remainder, b10UnitPosition, bxUnitValue, paddingValue, bxPredicate)
        (newPredicate, b10Remainder)
    else do
        let newPredicate = bxPredicate ++ [bxUnitValue]
        let nextUnitPosition = b10UnitPosition - 1
        bxCalcUnitValue b10Remainder nextUnitPosition newPredicate    

calcPredicate :: (Integer, Integer, Char, Char, String) -> String
calcPredicate (b10Remainder, b10UnitPosition, bxUnitValue, paddingValue, bxPredicate) = do
    if b10UnitPosition /= 0 
        then bxPredicate ++ [bxUnitValue] ++ replicate (fromIntegral b10UnitPosition) paddingValue 
        else bxPredicate ++ [bxUnitValue]

