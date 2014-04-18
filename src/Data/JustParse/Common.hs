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
    isFail,
    isPartial,
    (<?>),
    rename,

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
import Data.JustParse.Internal ( Stream(..), Parser(..), satisfy, Result(..), length, extend, mN, finalize, isDone, isPartial, isFail, rename, (<?>) )
import Data.Monoid ( mempty )
import Data.List ( minimumBy )
import Data.Char ( isControl, isSpace, isLower, isUpper, isAlpha, isAlphaNum, isPrint, 
                   isDigit, isOctDigit, isHexDigit, isLetter, isMark, isNumber, isPunctuation, 
                   isSymbol, isSeparator, isAscii, isLatin1, isAsciiUpper, isAsciiLower )
import Data.Ord ( comparing )
import Control.Monad ( void, (>=>) )
import Control.Applicative ( (<|>), optional, (<*) )

justParse :: Stream s t => Parser s t a -> s -> Maybe a
justParse p s = 
    case finalize (parse (greedy p) (Just s)) of
        [] -> Nothing
        (Done v _:_) -> Just v
        (Fail m _:_) -> Nothing

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
        b (Fail _ _) = True
        b _ = False
        f Nothing = 0
        f (Just s) = length s
        g [] = []
        g xs 
            | all b xs = 
                let
                    ds = filter isDone xs
                    dm = minimum (map (f . leftover) ds)
                    fs = filter isFail xs
                    fm = minimum (map (f . leftover) fs)
                in
                    if not (null ds)
                        then filter ((dm==) . f . leftover) ds
                        else filter ((fm==) . f . leftover) fs
            | otherwise = [Partial $ \s -> g $ extend s xs] 
                

many :: Stream s t => Parser s t a -> Parser s t [a]
many p = rename "many" (mN 0 (-1) p)

many1 :: Stream s t => Parser s t a -> Parser s t [a]
many1 p = rename "many1" (mN 1 (-1) p)

manyN :: Stream s t => Int -> Parser s t a -> Parser s t [a]
manyN n p = rename "manyN" (mN n (-1) p)

atLeast :: Stream s t => Int -> Parser s t a -> Parser s t [a]
atLeast n p = rename "atLeast" (mN n (-1) p)

exactly :: Stream s t => Int -> Parser s t a -> Parser s t [a]
exactly n p = rename "exactly" (mN n n p)

eof :: Stream s t => Parser s t ()
eof = Parser $ \s ->
    case s of
        Nothing -> [Done () s]
        Just s' -> 
            if s' == mempty
                then [Partial $ parse eof]
                else [Fail "eof" (Just s')]

takeTill :: Stream s t => Parser s t a -> Parser s t b -> Parser s t [a]
takeTill p e = rename "takeTill" (many p <* (void (lookAhead e) <|> void (lookAhead eof)))

takeTill1 :: Stream s t => Parser s t a -> Parser s t b -> Parser s t [a]
takeTill1 p e = rename "takeTill1" (many1 p <* (void (lookAhead e) <|> void (lookAhead eof)))

oneOf :: (Eq t, Stream s t) => [t] -> Parser s t t
oneOf ts = rename "oneOf" (satisfy (`elem` ts))

noneOf :: (Eq t, Stream s t) => [t] -> Parser s t t
noneOf ts = rename "noneOf" (satisfy (not . (`elem` ts)))

token :: (Eq t, Stream s t) => t -> Parser s t t
token t = rename "token" (satisfy (==t))

anyToken :: Stream s t => Parser s t t
anyToken = rename "anyToken" (satisfy (const True))

lookAhead :: Stream s t => Parser s t a -> Parser s t a
lookAhead (Parser p) = rename "lookAhead" $ Parser $ \s -> 
    let 
        g (Done a _) = [Done a s]
        g (Partial p') = [Partial $ p' >=> g]
    in
        p s >>= g

char :: Stream s Char => Char -> Parser s Char Char
char c = rename ("char "++[c]) (token c)

anyChar :: Stream s Char => Parser s Char Char
anyChar = rename "anyChar" anyToken

ascii :: Stream s Char => Parser s Char Char
ascii = rename "ascii" (satisfy isAscii)

latin1 :: Stream s Char => Parser s Char Char
latin1 = rename "latin1" (satisfy isLatin1)

control :: Stream s Char => Parser s Char Char
control = rename "control" (satisfy isControl)

space :: Stream s Char => Parser s Char Char
space = rename "space" (satisfy isSpace)

lower :: Stream s Char => Parser s Char Char
lower = rename "lower" (satisfy isLower)

upper :: Stream s Char => Parser s Char Char
upper = rename "upper" (satisfy isUpper)

alpha :: Stream s Char => Parser s Char Char
alpha = rename "alpha" (satisfy isAlpha)

alphaNum :: Stream s Char => Parser s Char Char
alphaNum = rename "alphaNum" (satisfy isAlphaNum)

print :: Stream s Char => Parser s Char Char
print = rename "print" (satisfy isPrint)

digit :: Stream s Char => Parser s Char Char
digit = rename "digit" (satisfy isDigit)

octDigit :: Stream s Char => Parser s Char Char
octDigit = rename "octDigit" (satisfy isOctDigit)

hexDigit :: Stream s Char => Parser s Char Char
hexDigit = rename "hexDigit" (satisfy isHexDigit)

string :: Stream s Char => String -> Parser s Char String
string s = rename ("string "++s) (mapM char s)

eol :: Stream s Char => Parser s Char String
eol = rename "eol" (string "\r\n" <|> string "\n\r" <|> string "\n")

line :: Stream s Char => Parser s Char String
line = rename "line" ((many1 (noneOf "\r\n") <* (void eol <|> void eof)) <|> (eol >> return ""))

word :: Stream s Char => Parser s Char String
word = rename "word" (optional (many1 space) >> takeTill1 (satisfy (not . isSpace)) space)

instance (Eq t) => Stream [t] t where
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)
