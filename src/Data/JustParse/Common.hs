{-|
Module      : Data.JustParse.Common
Description : Common Parser Combinators
Copyright   : Copyright Waived
License     : PublicDomain
Maintainer  : grantslatton@gmail.com
Stability   : experimental
Portability : portable

Many common parsing needs in one place.
-}

{-# LANGUAGE Safe #-}
module Data.JustParse.Common (
-- Parsing
    Stream(..),
    Result(..),
    Parser( parse ),
    finalize,
    extend,
    justParse,
    runParser,
    isDone,
    isPartial,

-- Primitive parsers
    satisfy,
    mN,
    mN',

-- Derived Parsers

-- Generic Parsers
    test,
    greedy,
    option,
    tryUntil,
    many,
    many',
    many1,
    many1',
    manyN,
    atLeast,
    exactly,
    sepBy,
    sepBy',
    sepBy1,
    sepBy1',
    eof,
    oneOf,
    noneOf,
    token,
    anyToken,
    lookAhead,

-- Char Parsers
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

-- String Parsers
    string,
    caseInsensitiveString,
    eol

) where

import Prelude hiding ( print, length )
import Data.JustParse.Internal ( Stream(..), Parser(..), Result(..), extend, finalize, isDone, isPartial )
import Data.Monoid ( mempty, Monoid )
import Data.Maybe ( fromMaybe )
import Data.List ( minimumBy )
import Data.Char ( isControl, isSpace, isLower, isUpper, isAlpha, isAlphaNum, isPrint, 
                   isDigit, isOctDigit, isHexDigit, isLetter, isMark, isNumber, isPunctuation, 
                   isSymbol, isSeparator, isAscii, isLatin1, isAsciiUpper, isAsciiLower,
                   toUpper, toLower )
import Data.Ord ( comparing )
import Control.Monad ( void, (>=>), liftM, mzero, liftM2 )
import Control.Applicative ( (<|>), optional, (<*) )

-- | Supplies the input to the 'Parser'. Returns all 'Result' types, 
-- including 'Partial' and 'Fail' types.
runParser :: Parser s a -> s -> [Result s a]
runParser p = parse p . Just

-- | This is a \"newbie\" command that one should probably only use out of frustration.
-- It runs the 'Parser' greedily over the input, 'finalize's all the results, and returns
-- the first successful result. If there are no successful results, it returns Nothing.
justParse :: Stream s t => Parser s a -> s -> Maybe a
justParse p s = 
    case finalize (parse (greedy p) (Just s)) of
        [] -> Nothing
        (Done v _:_) -> Just v

-- | Parse a token that satisfies a predicate.
satisfy :: Stream s t => (t -> Bool) -> Parser s t
satisfy f = Parser $ \s -> 
    case s of
        Nothing -> []
        Just s' -> case uncons s' of
            Nothing -> [Partial $ parse (satisfy f)]
            Just (x, xs) -> 
                if f x 
                    then [Done x (Just xs)]
                    else []

-- | Parse from @m@ to @n@ occurences of a 'Parser'. Let @n@ be negative
-- if one wishes for no upper bound.
mN :: Int -> Int -> Parser s a -> Parser s [a]
mN _ 0 _ = Parser $ \s -> [Done [] s] 
mN m n p = Parser $ \s -> 
    if m == 0 
        then Done [] s : (parse p s >>= g)
        else             parse p s >>= g
    where
        m' = if m == 0 then 0 else m-1
        g (Done a s) = parse (mN m' (n-1) p) s >>= h a
        g (Partial p') = [Partial $ p' >=> g]
        h a (Done as s) = [Done (a:as) s]
        h a (Partial p') = [Partial $ p' >=> h a]

mN' :: Stream s t => Int -> Int -> Parser s a -> Parser s [a]
mN' m n p = greedy (mN m n p)

-- | Return @True@ if the 'Parser' would succeed if one were to apply it,
-- otherwise, @False@.
test :: Parser s a -> Parser s Bool
test p = 
    do 
        a <- optional (lookAhead p)
        case a of
            Nothing -> return False
            _ -> return True

-- | Modifies a 'Parser' so that it will ony return the most consumptive
-- succesful results. If there are no successful results, it will only
-- return the most consumptive failures. One can use @greedy@ to emulate
-- parsers from @Parsec@ or @attoparsec@.
greedy :: Stream s t => Parser s a -> Parser s a
greedy (Parser p) = Parser $ \s -> g (p s) 
    where
        f Nothing = 0
        f (Just s) = length s
        g [] = []
        g xs 
            | all isDone xs = [minimumBy (comparing (f . leftover)) xs]
            | otherwise = [Partial $ \s -> g $ extend s xs] 

-- | Attempts to apply a parser and returns a default value if it fails.
option :: a -> Parser s a -> Parser s a
option v p = 
    do
        r <- optional p
        case r of
            Nothing -> return v
            Just v' -> return v'

tryUntil :: [Parser s a] -> Parser s a
tryUntil [] = mzero
tryUntil (Parser p:ps) = Parser $ \s ->
    case p s of
        [] -> parse (tryUntil ps) s
        xs -> xs

-- | Parse any number of occurences of the 'Parser'. Equivalent to @'mN' 0 (-1)@.
many :: Parser s a -> Parser s [a]
many = mN 0 (-1) 

-- | @greedy . many@, useful for emulating Parsec or Attoparsec
many' :: Stream s t => Parser s a -> Parser s [a]
many' = greedy . many

-- | Parse one or more occurence of the 'Parser'. Equivalent to @'mN' 1 (-1)@.
many1 :: Parser s a -> Parser s [a]
many1 = mN 1 (-1) 

-- | @greedy . many1@, useful for emulating Parsec or Attoparsec
many1' :: Stream s t => Parser s a -> Parser s [a]
many1' = greedy . many1

-- | Parse at least @n@ occurences of the 'Parser'. Equivalent to @'mN' n (-1)@.
manyN :: Int -> Parser s a -> Parser s [a]
manyN n = mN n (-1) 

-- | Identical to 'manyN', just a more intuitive name.
atLeast :: Int -> Parser s a -> Parser s [a]
atLeast n = mN n (-1) 

-- | Parse exactly @n@ occurences of the 'Parser'. Equivalent to @'mN' n n@.
exactly :: Int -> Parser s a -> Parser s [a]
exactly n = mN n n 

-- | @sepBy p s@ parsers any number of occurences of p separated by s
sepBy :: Parser s a -> Parser s b -> Parser s [a]
sepBy p s = sepBy1 p s <|> return []

sepBy' :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
sepBy' p s = tryUntil [sepBy1' p s, return []]

-- | @sepBy p s@ parsers at least 1 occurence of p separated by s
sepBy1 :: Parser s a -> Parser s b -> Parser s [a]
sepBy1 p s = liftM2 (:) p (many (s >> p))

sepBy1' :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
sepBy1' p s = liftM2 (:) p (many' (s >> p))

-- | Only succeeds when supplied with @Nothing@.
eof :: (Eq s, Monoid s) => Parser s ()
eof = Parser $ \s ->
    case s of
        Nothing -> [Done () s]
        Just s' -> 
            if s' == mempty
                then [Partial $ parse eof]
                else []

oneOf :: (Eq t, Stream s t) => [t] -> Parser s t
oneOf ts = satisfy (`elem` ts)

noneOf :: (Eq t, Stream s t) => [t] -> Parser s t
noneOf ts = satisfy (not . (`elem` ts))

-- | Parse a specific token.
token :: (Eq t, Stream s t) => t -> Parser s t
token t = satisfy (==t)

anyToken :: Stream s t => Parser s t
anyToken = satisfy (const True)

-- | Applies the parser and returns its result, but resets
-- the leftovers as if it consumed nothing.
lookAhead :: Parser s a -> Parser s a
lookAhead (Parser p) = Parser $ \s -> 
    let 
        g (Done a _) = [Done a s]
        g (Partial p') = [Partial $ p' >=> g]
    in
        p s >>= g

-- | Parse a specic char.
char :: Stream s Char => Char -> Parser s Char
char = token

-- | Char specific version of anyToken
anyChar :: Stream s Char =>  Parser s Char
anyChar = anyToken

-- | Parse a specific char, ignoring case.
caseInsensitiveChar :: Stream s Char => Char -> Parser s Char
caseInsensitiveChar c = tryUntil [char (toUpper c), char (toLower c)]

ascii :: Stream s Char =>  Parser s Char
ascii = satisfy isAscii

latin1 :: Stream s Char =>  Parser s Char
latin1 = satisfy isLatin1

control :: Stream s Char =>  Parser s Char
control = satisfy isControl

space :: Stream s Char =>  Parser s Char
space = satisfy isSpace

lower :: Stream s Char =>  Parser s Char
lower = satisfy isLower

upper :: Stream s Char =>  Parser s Char
upper = satisfy isUpper

alpha :: Stream s Char =>  Parser s Char
alpha = satisfy isAlpha

alphaNum :: Stream s Char =>  Parser s Char
alphaNum = satisfy isAlphaNum

print :: Stream s Char =>  Parser s Char
print = satisfy isPrint

digit :: Stream s Char =>  Parser s Char
digit = satisfy isDigit

octDigit :: Stream s Char =>  Parser s Char
octDigit = satisfy isOctDigit

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
eol = tryUntil [string "\r\n", string "\n\r", string "\n"]

-- | Makes common types such as Strings into a Stream.
instance (Eq t) => Stream [t] t where
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)
