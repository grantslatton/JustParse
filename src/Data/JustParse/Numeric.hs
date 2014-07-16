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
    decDigit,
    hexDigit,
    unsignedDecInt,
    unsignedDecInt_,
    unsignedHexInt,
    unsignedHexInt_,
    decInt,
    decInt_,
    hexInt,
    hexInt_,
    decFloat,
    decFloat_,
) where

{-# LANGUAGE Safe #-}
import Data.JustParse.Combinator
import Data.JustParse.Internal
import qualified Data.JustParse.Char as C
import Control.Monad ( liftM, liftM2 )
import Data.Char ( ord, digitToInt, toUpper, isDigit, isHexDigit )

-- | Parse a single decimal digit into an 'Int'.
decDigit :: Stream s Char => Parser s Int
decDigit = liftM digitToInt C.digit
{-# INLINE decDigit #-}

-- | Parse a single hexadecimal digit into an 'Int'.
hexDigit :: Stream s Char => Parser s Int
hexDigit = liftM digitToInt C.hexDigit
{-# INLINE hexDigit #-}

-- | Parse a series of decimal digits into an 'Int'.
unsignedDecInt :: Stream s Char => Parser s Int
unsignedDecInt = decDigit >>= g
    where
        g x = 
            do
                d <- decDigit
                g (x*10+d)
            <|> return x
{-# INLINE unsignedDecInt #-}

-- | Branching version of 'unsignedDecInt'.
unsignedDecInt_ :: Stream s Char => Parser s Int
unsignedDecInt_ = decDigit >>= g
    where
        g x = 
            do
                d <- decDigit
                g (x*10+d)
            <||> return x
{-# INLINE unsignedDecInt_ #-}

-- | Parse a series of hexadecimal digits into an 'Int'.
unsignedHexInt :: Stream s Char => Parser s Int
unsignedHexInt = hexDigit >>= g
    where
        g x = 
            do
                d <- hexDigit
                g (x*16+d)
            <|> return x
{-# INLINE unsignedHexInt #-}

-- | Branching version of 'unsignedHexInt'.
unsignedHexInt_ :: Stream s Char => Parser s Int
unsignedHexInt_ = hexDigit >>= g
    where
        g x = 
            do
                d <- hexDigit
                g (x*16+d)
            <||> return x
{-# INLINE unsignedHexInt_ #-}

-- | Parse a series of decimal digits into an 'Int' with an optional sign.
decInt :: Stream s Char => Parser s Int
decInt = 
    do
        sign <- optional (oneOf "-+")
        num <- unsignedDecInt
        case sign of
            Just '-' -> return (-num)
            _ -> return num
{-# INLINE decInt #-}

-- | Branching version of 'decInt'.
decInt_ :: Stream s Char => Parser s Int
decInt_ = 
    do
        sign <- optional (oneOf "-+")
        num <- unsignedDecInt_
        case sign of
            Just '-' -> return (-num)
            _ -> return num
{-# INLINE decInt_ #-}

-- | Parse a series of hexadecimal digits into an 'Int' with an optional 
-- sign.
hexInt :: Stream s Char => Parser s Int
hexInt = 
    do
        sign <- optional (oneOf "-+")
        num <- unsignedHexInt
        case sign of
            Just '-' -> return (-num)
            _ -> return num
{-# INLINE hexInt #-}

-- | Branching versino of 'hexInt'.
hexInt_ :: Stream s Char => Parser s Int
hexInt_ = 
    do
        sign <- optional (oneOf "-+")
        num <- unsignedHexInt_
        case sign of
            Just '-' -> return (-num)
            _ -> return num
{-# INLINE hexInt_ #-}

-- | Parse a float. If a decimal point is present, it must have at 
-- least one digit before and after the decimal point.
decFloat :: Stream s Char => Parser s Float
decFloat = 
    do
        sign <- optional (oneOf "-+")
        whole <- many1 C.digit
        fractional <- option ".0" (liftM2 (:) (C.char '.') (many1 C.digit))
        case sign of
            Just '-' -> return (-(read (whole ++ fractional)))
            _ -> return (read (whole ++ fractional))
{-# INLINE decFloat #-}

-- | Branching version of decFloat.
decFloat_ :: Stream s Char => Parser s Float
decFloat_ = 
    do
        sign <- optional (oneOf "-+")
        whole <- many1_ C.digit
        fractional <- option_ ".0" (liftM2 (:) (C.char '.') (many1_ C.digit))
        case sign of
            Just '-' -> return (-(read (whole ++ fractional)))
            _ -> return (read (whole ++ fractional))
{-# INLINE decFloat_ #-}
