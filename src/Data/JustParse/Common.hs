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

--{-# LANGUAGE Safe #-}
module Data.JustParse.Common (
-- Parsing
    Stream(..),
    Result(..),
    Parser( parse ),
    finalize,
    extend,
    parseOnly,
    runParser,
    isDone,
    isPartial,
    toPartial,

-- Primitive parsers
    satisfy,

-- Derived Parsers

-- Generic Parsers
    assert,
    test,
    greedy,
    option,
    choice,
    fork,
    (<||>),
    mN,
    many,
    many_,
    many1,
    many1_,
    sepBy,
    sepBy1,
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
import Data.JustParse.Internal ( 
    Stream(..), Parser(..), Result(..), extend, finalize, isDone, 
    isPartial, toPartial, streamAppend )
import Data.Monoid ( mempty, Monoid, mappend )
import Data.Maybe ( fromMaybe )
import Data.List ( minimumBy, foldl1' )
import Data.Char ( 
    isControl, isSpace, isLower, isUpper, isAlpha, isAlphaNum, isPrint, 
    isDigit, isOctDigit, isHexDigit, isLetter, isMark, isNumber, 
    isPunctuation, isSymbol, isSeparator, isAscii, isLatin1, isAsciiUpper, 
    isAsciiLower, toUpper, toLower )
import Data.Ord ( comparing )
import Control.Monad ( void, (>=>), liftM, mzero, liftM2 )
import Control.Applicative ( (<|>), optional, (<*), many, some )

-- | Parse a token that satisfies a predicate.
satisfy :: Stream s t => (t -> Bool) -> Parser s t
satisfy f = Parser $ \s -> 
    case s of
        Nothing -> []
        Just s' -> case uncons s' of
            Nothing -> [Partial $ parse (satisfy f)]
            Just (x, xs) -> [Done x (Just xs) | f x]

--mN :: Int -> Int -> Parser s a -> Parser s [a]
--mN _ 0 _ = Parser $ \s -> [Done [] s]
--mN m n p = Parser $ \s -> 

fork :: Parser s a -> Parser s a -> Parser s a
fork a b = Parser $ \s -> parse a s ++ parse b s

infixr 1 <||>
(<||>) :: Parser s a -> Parser s a -> Parser s a
(<||>) = fork

mN :: (Eq s, Monoid s) => Int -> Int -> Parser s a -> Parser s [a]
mN _ 0 _ = Parser $ \s -> [Done [] s]
mN 0 n p = liftM2 (:) p (mN 0 (n-1) p) <|> return []
mN m n p = liftM2 (:) p (mN (m-1) (n-1) p)

mN_ :: (Eq s, Monoid s) => Int -> Int -> Parser s a -> Parser s [a]
mN_ _ 0 _ = Parser $ \s -> [Done [] s]
mN_ 0 n p = liftM2 (:) p (mN 0 (n-1) p) <||> return []
mN_ m n p = liftM2 (:) p (mN (m-1) (n-1) p)

many_ :: Parser s a -> Parser s [a]
many_ p = return [] <||> liftM2 (:) p (many_ p)

many1 :: (Eq s, Monoid s) => Parser s a -> Parser s [a]
many1 = some

many1_ :: Parser s a -> Parser s [a]
many1_ p = liftM2 (:) p (many_ p)

assert :: (Eq s, Monoid s) => Bool -> Parser s ()
assert True = return ()
assert False = mzero

-- | Return @True@ if the 'Parser' would succeed if one were to apply it,
-- otherwise, @False@.
test :: (Eq s, Monoid s) => Parser s a -> Parser s Bool
test p = 
    do 
        a <- optional (lookAhead p)
        case a of
            Nothing -> return False
            _ -> return True

choice :: (Eq s, Monoid s) => [Parser s a] -> Parser s a
choice = foldl1' (<|>) 

choice_ :: (Eq s, Monoid s) => [Parser s a] -> Parser s a
choice_ = foldl1' (<||>)

-- | Modifies a 'Parser' so that it will ony return the most consumptive
-- succesful results. If there are no successful results, it will only
-- return the most consumptive failures. 
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
option :: (Eq s, Monoid s) => a -> Parser s a -> Parser s a
option v p = 
    do
        r <- optional p
        case r of
            Nothing -> return v
            Just v' -> return v'

-- | @sepBy p s@ parsers any number of occurences of p separated by s
sepBy :: (Eq s, Monoid s) => Parser s a -> Parser s b -> Parser s [a]
sepBy p s = sepBy1 p s <|> return []

sepBy_ :: (Eq s, Monoid s) => Parser s a -> Parser s b -> Parser s [a]
sepBy_ p s = sepBy1_ p s <||> return []

-- | @sepBy p s@ parsers at least 1 occurence of p separated by s
sepBy1 :: (Eq s, Monoid s) => Parser s a -> Parser s b -> Parser s [a]
sepBy1 p s = liftM2 (:) p (many (s >> p))

sepBy1_ :: (Eq s, Monoid s) => Parser s a -> Parser s b -> Parser s [a]
sepBy1_ p s = liftM2 (:) p (many_ (s >> p))

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
noneOf ts = satisfy (`notElem` ts)

-- | Parse a specific token.
token :: (Eq t, Stream s t) => t -> Parser s t
token t = satisfy (==t)

anyToken :: Stream s t => Parser s t
anyToken = satisfy (const True)

-- | Applies the parser and returns its result, but resets
-- the leftovers as if it consumed nothing.
lookAhead :: Monoid s => Parser s a -> Parser s a
lookAhead v@(Parser p) = Parser $ \s -> 
    let 
        g (Done a _) = Done a s
        g (Partial p') = Partial $ \s' -> 
            case s' of
                Nothing -> finalize (p' s)
                _ -> parse (lookAhead v) (streamAppend s s')
    in
        map g (p s)

-- | Parse a specic char.
char :: Stream s Char => Char -> Parser s Char
char = token

-- | Char specific version of anyToken
anyChar :: Stream s Char =>  Parser s Char
anyChar = anyToken

-- | Parse a specific char, ignoring case.
caseInsensitiveChar :: Stream s Char => Char -> Parser s Char
caseInsensitiveChar c = choice [char (toUpper c), char (toLower c)]

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
eol = choice [string "\r\n", string "\n\r", string "\n"]

-- | Makes common types such as Strings into a Stream.
instance (Eq t) => Stream [t] t where
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)
