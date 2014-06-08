module Data.JustParse.Numeric (
    decFloat,
    decInt,
    hexInt
) where

import Data.JustParse.Prim
import Data.JustParse.Combinator
import Data.JustParse.Char
import Control.Monad ( liftM, liftM2 )
import Data.Char ( ord, digitToInt, toUpper, isDigit, isHexDigit )



-- | Reads many decimal digits and returns them as an @Int@
decInt :: Stream s Char => Parser s Int
decInt = 
    do
        sign <- optional (oneOf "-+")
        num <- liftM read (many1 digit)
        case sign of
            Just '-' -> return (-num)
            _ -> return num

-- | Parse a float. If a decimal point is present, it must have at least 1 digit before and after the decimal point.
decFloat :: Stream s Char => Parser s Float
decFloat = 
    do
        sign <- optional (oneOf "-+")
        whole <- many1 digit
        fractional <- option ".0" (liftM2 (:) (char '.') (many1 digit))
        case sign of
            Just '-' -> return (-(read (whole ++ fractional)))
            _ -> return (read (whole ++ fractional))

-- | Reads many hexidecimal digits and returns them as an @Int@
hexInt :: Stream s Char => Parser s Int
hexInt = 
    do
        sign <- optional (oneOf "-+")
        num <- liftM (f . reverse) (many1 hexDigit)
        case sign of
            Just '-' -> return (-num)
            _ -> return num
    where
        f [] = 0
        f (x:xs) = if isDigit x then digitToInt x + 16 * f xs else ord (toUpper x) - ord 'A' + 16 * f xs
