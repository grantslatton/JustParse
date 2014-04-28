module Data.JustParse.Numeric (
    decInt,
    hexInt
) where

import Data.JustParse.Common
import Control.Monad ( liftM )
import Control.Applicative ( optional )
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
