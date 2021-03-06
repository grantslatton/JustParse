{-|
Module      : Data.JustParse.Language
Description : Regular expressions
Copyright   : Copyright Waived
License     : PublicDomain
Maintainer  : grantslatton@gmail.com
Stability   : experimental
Portability : portable

Allows for conversion from a regular expression and a 'Parser'.
-}

{-# LANGUAGE Safe #-}
module Data.JustParse.Language (
      Match (..)
    , regex
    , regex_
    , regex'
    , regex_'
) where

import Data.JustParse
import Data.JustParse.Combinator
import Data.JustParse.Numeric
import Data.JustParse.Char

import Control.Monad ( liftM, void )
import Data.Monoid ( Monoid, mconcat, mempty, mappend )
import Control.Applicative ( (<*), (<$) )

-- | @regex@ takes a regular expression in the form of a 'String' and,
-- if the regex is valid, returns a greedy 'Parser' that parses that regex.
-- If the regex is invalid, it returns Nothing.
regex :: Stream s Char => String -> Maybe (Parser s Match)
regex = liftM greedy . regex_
{-# INLINE regex #-}

-- | Like 'regex', but returns a branching (non-greedy) parser.
regex_ :: Stream s Char => String -> Maybe (Parser s Match)
regex_ = parseOnly (regular <* eof)
{-# INLINE regex_ #-}

-- | The same as 'regex', but only returns the full matched text.
regex' :: Stream s Char => String -> Maybe (Parser s String)
regex' = liftM (liftM matched) . regex
{-# INLINE regex' #-}

-- | The same as 'regex_', but only returns the full matched text.
regex_' :: Stream s Char => String -> Maybe (Parser s String)
regex_' = liftM (liftM matched) . regex_
{-# INLINE regex_' #-}

-- | The result of a 'regex'
data Match = 
    Match {
        -- | The complete text matched within the regex
          matched :: String
        -- | Any submatches created by using capture groups
        , groups :: [Match]
    } deriving (Show, Eq)

-- mconcat makes things very nice for concatenating the results of subregexes
instance Monoid Match where
    mempty = Match "" []
    mappend (Match m g) (Match m' g') = Match { matched = m ++ m', groups = g ++ g' }

regular :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
regular = liftM (liftM mconcat . sequence) (many parser)
{-# INLINE regular #-}

-- all parsers
parser :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
parser = choice [
      asterisk
    , mn
    , pipe
    , plus
    , question
    , lookAheadAssert
    , lookAheadAssertNot
    , group
    , character
    , charClass
    , negCharClass
    , period
    , endOfLine
    ]
{-# INLINE parser #-}

-- parsers with no pipe
parserNP :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
parserNP = choice [
      asterisk
    , mn
    , plus
    , question
    , lookAheadAssert
    , lookAheadAssertNot
    , group
    , character
    , charClass
    , negCharClass
    , period
    ]
{-# INLINE parserNP #-}

-- parsers that can be followed by special operators (i.e. to prevent a 
-- regex like "a???*?" which would be hazardous)
restricted :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
restricted = choice [
      character
    , charClass
    , negCharClass
    , lookAheadAssert
    , lookAheadAssertNot
    , group
    , period
    ]
{-# INLINE restricted #-}

unreserved :: Stream s Char => Parser s Char 
unreserved = (char '\\' >> anyChar ) <|> noneOf "()[]\\*+{}^?|.$"
{-# INLINE unreserved #-}

character :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
character = do
    c <- unreserved
    return $ do
        char c
        return $ Match [c] []
{-# INLINE character #-}

charClass :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
charClass = do
    char '['
    c <- many1 unreserved
    char ']'
    return $ do
        c' <- oneOf c
        return $ Match [c'] []
{-# INLINE charClass #-}

negCharClass :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
negCharClass = do
    string "[^"
    c <- many1 unreserved
    char ']'
    return $ do
        c' <- noneOf c
        return $ Match [c'] []
{-# INLINE negCharClass #-}

period :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
period = do
    char '.'
    return $ do
        c <- noneOf "\n\r"
        return $ Match [c] []
{-# INLINE period #-}


question :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
question = do
    p <- restricted
    char '?'
    return $ liftM mconcat (mN_ 0 1 p)
{-# INLINE question #-}

group :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
group = do
    char '('
    p <- regular
    char ')'
    return $ do
        r <- p
        return $ r { groups = [r] } 
{-# INLINE group #-}

asterisk :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
asterisk = do
    p <- restricted
    char '*'
    return $ liftM mconcat (many_ p)
{-# INLINE asterisk #-}

plus :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
plus = do
    p <- restricted
    char '+'
    return $ liftM mconcat (many1_ p)
{-# INLINE plus #-}

mn :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
mn = do
    p <- restricted
    char '{'
    l <- option 0 decInt
    char ','
    r <- option (-1) decInt
    char '}'
    return $ liftM mconcat (mN_ l r p)
{-# INLINE mn #-}

pipe :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
pipe = do
    p <- parserNP
    char '|'
    p' <- parser
    return $ p <||> p'
{-# INLINE pipe #-}

-- (?=pattern) : positive look-ahead assertion
lookAheadAssert :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
lookAheadAssert = do
    string "(?="
    p <- regular
    char ')'
    return $ do
        lookAhead p
        return $ Match "" []
{-# INLINE lookAheadAssert #-}

-- (?!pattern) : negative look-ahead assertion
lookAheadAssertNot :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
lookAheadAssertNot = do
    string "(?!"
    p <- regular
    char ')'
    return $ do
        assertNotP $ lookAhead p
        return $ Match "" []
{-# INLINE lookAheadAssertNot #-}

endOfLine :: (Stream s0 Char, Stream s1 Char) => Parser s0 (Parser s1 Match)
endOfLine = do
    char '$' >> eof
    return $ Match "" [] <$ (void eol <|> eof)
{-# INLINE endOfLine #-}
