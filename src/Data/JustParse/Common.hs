module Data.JustParse.Common (
-- Parsing
    Stream (..),
    Result (..),
    Parser,
    finalize,
    extend,
    parse,
    justParse,
    runParser,


-- Primitive parsers
    satisfy,
    mN,

-- Derived Parsers

-- Generic Parsers
    test,
    greedy,
    many,
    many1,
    manyN,
    atLeast,
    exactly,
    eof,
    takeTill,
    takeTill1,
    oneOf,
    noneOf,
    anyToken,
    lookAhead,

-- Char Parsers
    char,
    anyChar,
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
    eol,

-- String Parsers
    string,
    line,
    word,

) where

import Prelude hiding ( print, length )
import Data.JustParse.Internal 
import Data.Monoid
import Data.List hiding ( length )
import Data.Char
import Data.Ord
import Data.Maybe
import Control.Monad
import Control.Applicative ((<|>), (<*), optional )

justParse :: Stream s t => Parser s t a -> s -> Maybe a
justParse p s = 
    case finalize (parse (greedy p) (Just s)) of
        [] -> Nothing
        (x:_) -> Just (value x)

runParser :: Stream s t => Parser s t a -> s -> [Result s t a]
runParser p = parse p . Just

test :: Stream s t => Parser s t a -> Parser s t Bool
test p = 
    do 
        a <- optional (lookAhead p)
        case a of
            Nothing -> return False
            _ -> return True

greedy :: Stream s t => Parser s t a -> Parser s t a
greedy (Parser p) = Parser $ \s -> g (p s) 
    where
        b (Done _ _) = True
        b _ = False
        f Nothing = 0
        f (Just s) = length s
        g [] = []
        g xs 
            | all b xs = [minimumBy (comparing (f . leftover)) xs] 
            | otherwise = [Partial $ \s -> g $ extend s xs] 

many :: Stream s t => Parser s t a -> Parser s t [a]
many = mN 0 (-1)

many1 :: Stream s t => Parser s t a -> Parser s t [a]
many1 = mN 1 (-1)

manyN :: Stream s t => Int -> Parser s t a -> Parser s t [a]
manyN = flip mN (-1)

atLeast :: Stream s t => Int -> Parser s t a -> Parser s t [a]
atLeast = manyN

exactly :: Stream s t => Int -> Parser s t a -> Parser s t [a]
exactly n = mN n n

eof :: Stream s t => Parser s t ()
eof = Parser $ \s ->
    case s of
        Nothing -> [Done () s]
        Just s' -> [Partial $ parse eof | s' == mempty]

takeTill :: Stream s t => Parser s t a -> Parser s t b -> Parser s t [a]
takeTill p e = many p <* (void (lookAhead e) <|> void (lookAhead eof))

takeTill1 :: Stream s t => Parser s t a -> Parser s t b -> Parser s t [a]
takeTill1 p e = many1 p <* (void (lookAhead e) <|> void (lookAhead eof))

oneOf :: (Eq t, Stream s t) => [t] -> Parser s t t
oneOf ts = satisfy (`elem` ts)

noneOf :: (Eq t, Stream s t) => [t] -> Parser s t t
noneOf ts = satisfy (not . (`elem` ts))

token :: (Eq t, Stream s t) => t -> Parser s t t
token t = Parser $ parse $ satisfy (==t)

anyToken :: Stream s t => Parser s t t
anyToken = satisfy (const True)

lookAhead :: Stream s t => Parser s t a -> Parser s t a
lookAhead (Parser p) = Parser $ \s -> 
    let 
        g (Done a _) = [Done a s]
        g (Partial p') = [Partial $ p' >=> g]
    in
        p s >>= g

char :: Stream s Char => Char -> Parser s Char Char
char = token 

anyChar :: Stream s Char => Parser s Char Char
anyChar = anyToken

ascii :: Stream s Char => Parser s Char Char
ascii = satisfy isAscii

latin1 :: Stream s Char => Parser s Char Char
latin1 = satisfy isLatin1

control :: Stream s Char => Parser s Char Char
control = satisfy isControl

space :: Stream s Char => Parser s Char Char
space = satisfy isSpace

lower :: Stream s Char => Parser s Char Char
lower = satisfy isLower

upper :: Stream s Char => Parser s Char Char
upper = satisfy isUpper

alpha :: Stream s Char => Parser s Char Char
alpha = satisfy isAlpha

alphaNum :: Stream s Char => Parser s Char Char
alphaNum = satisfy isAlphaNum

print :: Stream s Char => Parser s Char Char
print = satisfy isPrint

digit :: Stream s Char => Parser s Char Char
digit = satisfy isDigit

octDigit :: Stream s Char => Parser s Char Char
octDigit = satisfy isOctDigit

hexDigit :: Stream s Char => Parser s Char Char
hexDigit = satisfy isHexDigit

string :: Stream s Char => String -> Parser s Char String
string = mapM char

eol :: Stream s Char => Parser s Char String
eol = string "\r\n" <|> string "\n\r" <|> string "\n"

line :: Stream s Char => Parser s Char String
line = (many1 (noneOf "\r\n") <* (void eol <|> void eof)) <|> (eol >> return "")

word :: Stream s Char => Parser s Char String
word = optional (many1 space) >> takeTill1 (satisfy (not . isSpace)) space

instance (Eq t) => Stream [t] t where
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)
