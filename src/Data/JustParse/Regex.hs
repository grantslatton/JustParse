module Data.JustParse.Regex (
    Match (..),
    regex
) where

import Data.JustParse.Common
import Control.Applicative ( (<|>), optional )
import Prelude 
import Data.Monoid
import Data.List (intercalate)

import Debug.Trace
prnt = flip trace

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

regex :: Stream s Char => Parser s Char (Parser s Char Match)
regex = 
    do
        ps <- greedy (many parser)
        return $ do
            rs <- sequence ps
            return (mconcat rs)

parser :: Stream s Char => Parser s Char (Parser s Char Match)
parser = character <|> charClass <|> negCharClass <|> question <|> group <|> asterisk <|> plus <|> mn <|> period <|> pipe

parserNP :: Stream s Char => Parser s Char (Parser s Char Match)
parserNP = character <|> charClass <|> negCharClass <|> question <|> group <|> asterisk <|> plus <|> mn <|> period 

restricted :: Stream s Char => Parser s Char (Parser s Char Match)
restricted = character <|> charClass <|> negCharClass <|> group <|> period

unreserved :: Stream s Char => Parser s Char Char
unreserved = (char '\\' >> anyChar ) <|> noneOf "()[]\\*+{}^?:<>|"

character :: Stream s Char => Parser s Char (Parser s Char Match)
character = 
    do
        c <- unreserved
        return $ do
            c' <- char c
            return $ Match [c] []

charClass :: Stream s Char => Parser s Char (Parser s Char Match)
charClass = 
    do
        char '['
        c <- greedy (many1 unreserved)
        char ']'
        return $ do
            c' <- oneOf c
            return $ Match [c'] []

negCharClass :: Stream s Char => Parser s Char (Parser s Char Match)
negCharClass = 
    do
        string "[^"
        c <- greedy (many1 unreserved)
        char ']'
        return $ do
            c' <- noneOf c
            return $ Match [c'] []

period :: Stream s Char => Parser s Char (Parser s Char Match)
period = 
    do
        char '.'
        return $ do
            c <- noneOf "\n\r"
            return $ Match [c] []


question :: Stream s Char => Parser s Char (Parser s Char Match)
question = 
    do
        p <- restricted
        char '?'
        return $ do
            r <- mN 0 1 p
            return $ mconcat r

group :: Stream s Char => Parser s Char (Parser s Char Match)
group = 
    do
        string "("
        p <- regex
        char ')'
        return $ do
            r <- p
            return $ r { groups = [r] } 

asterisk :: Stream s Char => Parser s Char (Parser s Char Match)
asterisk = 
    do
        p <- restricted
        char '*'
        return $ do
            r <- many p
            return $ mconcat r

plus :: Stream s Char => Parser s Char (Parser s Char Match)
plus = 
    do
        p <- restricted
        char '+'
        return $ do
            r <- many1 p
            return $ mconcat r

mn :: Stream s Char => Parser s Char (Parser s Char Match)
mn = 
    do
        p <- restricted
        char '{'
        l <- optional (many1 digit)
        char ','
        r <- optional (many1 digit)
        char '}'
        return $ do
            r <- mN (maybe 0 read l) (maybe (-1) read r) p
            return $ mconcat r

pipe :: Stream s Char => Parser s Char (Parser s Char Match)
pipe = 
    do
        p <- parserNP
        char '|'
        p' <- parser
        return $ do
            r <- p <|> p'
            return $ r
