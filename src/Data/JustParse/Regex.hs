module Data.JustParse.Regex (
    Match (..),
    regex
) where

import Data.JustParse.Common ( char, string, many1, digit, Stream, noneOf, oneOf, greedy, many, mN, anyChar, leftover, value, finalize, parse, Result(..) )
import Data.JustParse.Internal( Parser (..) )
import Control.Applicative ( (<|>), optional )
import Control.Monad ( liftM, mzero )
import Data.Monoid ( Monoid, mconcat, mempty, mappend )
import Data.Maybe ( isJust )
import Data.List ( intercalate )

-- Takes a regex in the form of a string, parses the regex, 
-- and returns a Parser that parses that regex. If the regex
-- is invalid, the parser will only return failures informing one of such things.
regex :: Stream s Char => String -> Parser s Match
regex s 
    | null r = Parser $ \s -> [Fail "Invalid Regex" s]
    | isJust $ leftover $ head r = Parser $ \s -> [Fail "Invalid Regex" s]
    | otherwise = value $ head r
    where
        r = finalize (parse (greedy regular) (Just s))

-- A type that contains the matche text within it, and any subgroups
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

-- mconcat makes things very nice for concatenating the results of subregexes
instance Monoid Match where
    mempty = Match "" []
    mappend (Match m g) (Match m' g') = 
        Match {
            matched = m ++ m',
            groups = g ++ g'
        }

-- Parses a regex and returns the resulting parser
regular :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
regular = liftM (liftM mconcat . sequence) (greedy $ many parser)

parser :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
parser = character <|> charClass <|> negCharClass <|> question <|> group <|> asterisk <|> plus <|> mn <|> period <|> pipe

-- A parser alternation with no Pipe
parserNP :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
parserNP = character <|> charClass <|> negCharClass <|> question <|> group <|> asterisk <|> plus <|> mn <|> period 

-- Non left recursive parsers
restricted :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
restricted = character <|> charClass <|> negCharClass <|> group <|> period

-- Parses tokens. Either an escaped (with a '\') or a literal
unreserved :: Stream s Char => Parser s Char 
unreserved = (char '\\' >> anyChar ) <|> noneOf "()[]\\*+{}^?:<>|."

character :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
character = 
    do
        c <- unreserved
        return $ do
            c' <- char c
            return $ Match [c] []

charClass :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
charClass = 
    do
        char '['
        c <- greedy (many1 unreserved)
        char ']'
        return $ do
            c' <- oneOf c
            return $ Match [c'] []

negCharClass :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
negCharClass = 
    do
        string "[^"
        c <- greedy (many1 unreserved)
        char ']'
        return $ do
            c' <- noneOf c
            return $ Match [c'] []

period :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
period = 
    do
        char '.'
        return $ do
            c <- noneOf "\n\r"
            return $ Match [c] []


question :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
question = 
    do
        p <- restricted
        char '?'
        return $ liftM mconcat (mN 0 1 p)

group :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
group = 
    do
        string "("
        p <- regular
        char ')'
        return $ do
            r <- p
            return $ r { groups = [r] } 

asterisk :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
asterisk = 
    do
        p <- restricted
        char '*'
        return $ liftM mconcat (many p)

plus :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
plus = 
    do
        p <- restricted
        char '+'
        return $ liftM mconcat (many1 p)

mn :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
mn = 
    do
        p <- restricted
        char '{'
        l <- optional (many1 digit)
        char ','
        r <- optional (many1 digit)
        char '}'
        return $ liftM mconcat (mN (maybe 0 read l) (maybe (-1) read r) p)

pipe :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
pipe = 
    do
        p <- parserNP
        char '|'
        p' <- parser
        return $ p <|> p'
