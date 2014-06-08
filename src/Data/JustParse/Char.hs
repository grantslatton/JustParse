{-|
Module      : Data.JustParse.Common
Description : Common Char Parsers
Copyright   : Copyright Waived
License     : PublicDomain
Maintainer  : grantslatton@gmail.com
Stability   : experimental
Portability : portable

Many common parsing needs in one place.
-}

--{-# LANGUAGE Safe #-}
module Data.JustParse.Char (
    char,
    anyChar,
    caseInsensitiveChar,
    ascii,
    latin1,
    control,
    space,
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
import Data.JustParse.Prim
import Data.JustParse.Combinator

-- | Parse a specic char.
char :: Stream s Char => Char -> Parser s Char
char = token

-- | Parse any char.
anyChar :: Stream s Char =>  Parser s Char
anyChar = anyToken

-- | Parse a specific char, ignoring case.
caseInsensitiveChar :: Stream s Char => Char -> Parser s Char
caseInsensitiveChar c = char (toUpper c) <|> char (toLower c)

-- | Parse any char that is ASCII.
ascii :: Stream s Char =>  Parser s Char
ascii = satisfy isAscii

-- | Parse any char that is Latin-1.
latin1 :: Stream s Char =>  Parser s Char
latin1 = satisfy isLatin1

-- | Parse a control character.
control :: Stream s Char =>  Parser s Char
control = satisfy isControl

-- | Parse a space.
space :: Stream s Char =>  Parser s Char
space = satisfy isSpace

-- | Parse a lowercase character.
lower :: Stream s Char =>  Parser s Char
lower = satisfy isLower

-- | Parse an uppercase character.
upper :: Stream s Char =>  Parser s Char
upper = satisfy isUpper

-- | Parse an alphabetic character.
alpha :: Stream s Char =>  Parser s Char
alpha = satisfy isAlpha

-- | Parse an alphanumeric character.
alphaNum :: Stream s Char =>  Parser s Char
alphaNum = satisfy isAlphaNum

-- | Parse a printable (non-control) character.
print :: Stream s Char =>  Parser s Char
print = satisfy isPrint

-- | Parse a digit.
digit :: Stream s Char =>  Parser s Char
digit = satisfy isDigit

-- | Parse an octal digit.
octDigit :: Stream s Char =>  Parser s Char
octDigit = satisfy isOctDigit

-- | Parse a hexadeciaml digit.
hexDigit :: Stream s Char =>  Parser s Char
hexDigit = satisfy isHexDigit

-- | Parse a specific string.
string :: Stream s Char => String -> Parser s String
string = mapM char 

-- | Parse a specfic string, ignoring case.
caseInsensitiveString :: Stream s Char => String -> Parser s String
caseInsensitiveString = mapM caseInsensitiveChar 

-- | Parses until a newline, carriage return + newline, or newline + carriage return.
eol :: Stream s Char => Parser s String
eol = choice [string "\r\n", string "\n\r", string "\n"]

-- | Makes common types such as Strings into a Stream.
instance (Eq t) => Stream [t] t where
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)
