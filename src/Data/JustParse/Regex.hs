module Data.JustParse.Regex (
    Match (..),
    regex
) where

import Data.JustParse.Common ( char, string, many1, digit, Parser, Stream, noneOf, oneOf, greedy, many, mN, anyChar, leftover, value, finalize, parse )
import Control.Applicative ( (<|>), optional )
import Control.Monad ( liftM, mzero )
import Data.Monoid ( Monoid, mconcat, mempty, mappend )
import Data.Maybe ( isJust )
import Data.List ( intercalate )

regex :: Stream s Char => String -> Parser s Char Match
regex s 
    | null r = mzero
    | isJust $ leftover $ head r = mzero
    | otherwise = value $ head r
    where
        r = finalize (parse (greedy regular) (Just s))

data Match = 
    Match {
        matched :: String,
        groups :: [Match]
    } 

instance Show Match where
    show = show' ""
        where
            show' i (Match m []) = i ++ m
            show' i (Match m gs) = i ++ m ++ "\n" ++ intercalate "\n" (map (show' ('\t':i)) gs)

instance Monoid Match where
    mempty = Match "" []
    mappend (Match m g) (Match m' g') = 
        Match {
            matched = m ++ m',
            groups = g ++ g'
        }

regular :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
regular = liftM (liftM mconcat . sequence) (greedy $ many parser)

parser :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
parser = character <|> charClass <|> negCharClass <|> question <|> group <|> asterisk <|> plus <|> mn <|> period <|> pipe

parserNP :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
parserNP = character <|> charClass <|> negCharClass <|> question <|> group <|> asterisk <|> plus <|> mn <|> period 

restricted :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
restricted = character <|> charClass <|> negCharClass <|> group <|> period

unreserved :: Stream s Char => Parser s Char Char
unreserved = (char '\\' >> anyChar ) <|> noneOf "()[]\\*+{}^?:<>|."

character :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
character = 
    do
        c <- unreserved
        return $ do
            c' <- char c
            return $ Match [c] []

charClass :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
charClass = 
    do
        char '['
        c <- greedy (many1 unreserved)
        char ']'
        return $ do
            c' <- oneOf c
            return $ Match [c'] []

negCharClass :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
negCharClass = 
    do
        string "[^"
        c <- greedy (many1 unreserved)
        char ']'
        return $ do
            c' <- noneOf c
            return $ Match [c'] []

period :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
period = 
    do
        char '.'
        return $ do
            c <- noneOf "\n\r"
            return $ Match [c] []


question :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
question = 
    do
        p <- restricted
        char '?'
        return $ liftM mconcat (mN 0 1 p)

group :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
group = 
    do
        string "("
        p <- regular
        char ')'
        return $ do
            r <- p
            return $ r { groups = [r] } 

asterisk :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
asterisk = 
    do
        p <- restricted
        char '*'
        return $ liftM mconcat (many p)

plus :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
plus = 
    do
        p <- restricted
        char '+'
        return $ liftM mconcat (many1 p)

mn :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
mn = 
    do
        p <- restricted
        char '{'
        l <- optional (many1 digit)
        char ','
        r <- optional (many1 digit)
        char '}'
        return $ liftM mconcat (mN (maybe 0 read l) (maybe (-1) read r) p)

pipe :: (Stream s0 Char, Stream s1 Char) => Parser s0 Char (Parser s1 Char Match)
pipe = 
    do
        p <- parserNP
        char '|'
        p' <- parser
        return $ p <|> p'
