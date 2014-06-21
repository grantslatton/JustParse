{-|
Module      : Data.JustParse.Numeric
Description : Numeric parsing
Copyright   : Copyright Waived
License     : PublicDomain
Maintainer  : grantslatton@gmail.com
Stability   : experimental
Portability : portable

Parsers for dealing with signed and unsigned 'Int's and 'Float's.
-}

module Data.JustParse.Numeric (
    decFloat,
    decFloat_,
    decInt,
    decInt_,
    hexInt,
    hexInt_
) where

--{-# LANGUAGE Safe #-}
import Data.JustParse.Combinator
import Data.JustParse.Internal
import Data.JustParse.Char
import Control.Monad ( liftM, liftM2 )
import Data.Char ( ord, digitToInt, toUpper, isDigit, isHexDigit )

-- | Reads many decimal digits and returns them as an @Int@.
decInt :: Stream s Char => Parser s Int
decInt = 
    do
        sign <- optional (oneOf "-+")
        num <- liftM read (many1 digit)
        case sign of
            Just '-' -> return (-num)
            _ -> return num

-- | Branching version of decInt.
decInt_ :: Stream s Char => Parser s Int
decInt_ = 
    do
        sign <- optional (oneOf "-+")
        num <- liftM read (many1_ digit)
        case sign of
            Just '-' -> return (-num)
            _ -> return num

-- | Parse a float. If a decimal point is present, it must have at 
-- least one digit before and after the decimal point.
decFloat :: Stream s Char => Parser s Float
decFloat = 
    do
        sign <- optional (oneOf "-+")
        whole <- many1 digit
        fractional <- option ".0" (liftM2 (:) (char '.') (many1 digit))
        case sign of
            Just '-' -> return (-(read (whole ++ fractional)))
            _ -> return (read (whole ++ fractional))

-- | Branching version of decFloat.
decFloat_ :: Stream s Char => Parser s Float
decFloat_ = 
    do
        sign <- optional (oneOf "-+")
        whole <- many1_ digit
        fractional <- option_ ".0" (liftM2 (:) (char '.') (many1_ digit))
        case sign of
            Just '-' -> return (-(read (whole ++ fractional)))
            _ -> return (read (whole ++ fractional))


-- | Reads many hexidecimal digits and returns them as an @Int@.
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
        f (x:xs) 
            | isDigit x = digitToInt x + 16 * f xs 
            | otherwise = ord (toUpper x) - ord 'A' + 16 * f xs

-- | Branching versino of 'hexInt'.
hexInt_ :: Stream s Char => Parser s Int
hexInt_ = 
    do
        sign <- optional (oneOf "-+")
        num <- liftM (f . reverse) (many1_ hexDigit)
        case sign of
            Just '-' -> return (-num)
            _ -> return num
    where
        f [] = 0
        f (x:xs) 
            | isDigit x = digitToInt x + 16 * f xs 
            | otherwise = ord (toUpper x) - ord 'A' + 16 * f xs
