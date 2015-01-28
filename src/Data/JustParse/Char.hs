{-|
Module      : Data.JustParse.Common
Description : Common char parsers
Copyright   : Copyright Waived
License     : PublicDomain
Maintainer  : grantslatton@gmail.com
Stability   : experimental
Portability : portable

Several useful parsers for dealing with 'String's.
-}

{-# LANGUAGE Safe #-}
module Data.JustParse.Char (
    char,
    anyChar,
    caseInsensitiveChar,
    ascii,
    latin1,
    control,
    space,
    spaces,
    lower,
    upper,
    alpha,
    alphaNum,
    print,
    digit,
    octDigit,
    hexDigit,
    string,
    caseInsensitiveString,
    eol
) where

import Prelude hiding (print)
import Data.Char
import Data.JustParse.Internal
import Data.JustParse.Combinator

-- | Parse a specic char.
char :: Stream s Char => Char -> Parser s Char
char = token
{-# INLINE char #-}

-- | Parse any char.
anyChar :: Stream s Char =>  Parser s Char
anyChar = anyToken
{-# INLINE anyChar #-}

-- | Parse a specific char, ignoring case.
caseInsensitiveChar :: Stream s Char => Char -> Parser s Char
caseInsensitiveChar c = char (toUpper c) <|> char (toLower c)
{-# INLINE caseInsensitiveChar #-}

-- | Parse any char that is ASCII.
ascii :: Stream s Char =>  Parser s Char
ascii = satisfy isAscii
{-# INLINE ascii #-}

-- | Parse any char that is Latin-1.
latin1 :: Stream s Char =>  Parser s Char
latin1 = satisfy isLatin1
{-# INLINE latin1 #-}

-- | Parse a control character.
control :: Stream s Char =>  Parser s Char
control = satisfy isControl
{-# INLINE control #-}

-- | Parse a space.
space :: Stream s Char =>  Parser s Char
space = satisfy isSpace
{-# INLINE space #-}

-- | Parse many spaces.
spaces :: Stream s Char => Parser s [Char]
spaces = many space
{-# INLINE spaces #-}

-- | Parse a lowercase character.
lower :: Stream s Char =>  Parser s Char
lower = satisfy isLower
{-# INLINE lower #-}

-- | Parse an uppercase character.
upper :: Stream s Char =>  Parser s Char
upper = satisfy isUpper
{-# INLINE upper #-}

-- | Parse an alphabetic character.
alpha :: Stream s Char =>  Parser s Char
alpha = satisfy isAlpha
{-# INLINE alpha #-}

-- | Parse an alphanumeric character.
alphaNum :: Stream s Char =>  Parser s Char
alphaNum = satisfy isAlphaNum
{-# INLINE alphaNum #-}

-- | Parse a printable (non-control) character.
print :: Stream s Char =>  Parser s Char
print = satisfy isPrint
{-# INLINE print #-}

-- | Parse a digit.
digit :: Stream s Char =>  Parser s Char
digit = satisfy isDigit
{-# INLINE digit #-}

-- | Parse an octal digit.
octDigit :: Stream s Char =>  Parser s Char
octDigit = satisfy isOctDigit
{-# INLINE octDigit #-}

-- | Parse a hexadeciaml digit.
hexDigit :: Stream s Char =>  Parser s Char
hexDigit = satisfy isHexDigit
{-# INLINE hexDigit #-}

-- | Parse a specific string.
string :: Stream s Char => String -> Parser s String
string = mapM char 
{-# INLINE string #-}

-- | Parse a specfic string, ignoring case.
caseInsensitiveString :: Stream s Char => String -> Parser s String
caseInsensitiveString = mapM caseInsensitiveChar 
{-# INLINE caseInsensitiveString #-}

-- | Parses until a newline, carriage return, carriage return + newline, or newline + carriage return.
eol :: Stream s Char => Parser s String
eol = choice [string "\r\n", string "\n\r", string "\n", string "\r"]
{-# INLINE eol #-}

-- | Makes common types such as 'String's into a Stream.
instance Eq t => Stream [t] t where
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)
