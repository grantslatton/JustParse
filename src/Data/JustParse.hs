{-|
Module      : Data.JustParse
Description : Parsing functions and types
Copyright   : Copyright Waived
License     : PublicDomain
Maintainer  : grantslatton@gmail.com
Stability   : experimental
Portability : portable
-} 

{-# LANGUAGE Safe #-}
module Data.JustParse (
    -- * Overview
    -- $overview

    -- * Quickstart Examples
    -- *** Simple char and string parsing
    -- $quickstart1

    -- *** Basic combinatorial parsing
    -- $quickstart2

    -- *** Recursive combinatorial parsing
    -- $quickstart3
    runParser,
    parseOnly,
    extend,
    finalize,
    Parser,
    Result(..),
    Stream(..)
) where

import Data.JustParse.Internal
import Data.JustParse.Combinator

-- $overview
-- 
-- * Allows for parsing arbitrary 'Stream' types
-- 
-- * Makes extensive use of combinators
-- 
-- * Allows for a parser to return a 'Partial' result
-- 
-- * Returns a list of all possible parses
-- 
-- * Allows for conversion of a 'regex' to a parser

-- $quickstart1
--
-- This parser will only accept the string @\"hello world\"@
--
-- @
--p = do                
--    h \<- char \'h\'                   \-\-Parses the character \'h\'
--    rest \<- string \"ello world\"     \-\-Parses the string \"ello world\"
--    ex \<- char \'!\'                  \-\-Parses the character \'!\'
--    return ([h]++rest++[ex])        \-\-Returns all of the above concatenated together
-- @

-- $quickstart2
--
-- This parser will accept the string @\"hello woooooorld\"@ with any number of @o@\'s.
-- It returns the number of @o@\'s.
--
-- @
--p = do
--    string \"hello w\"         \-\-Parses the string \"hello w\"
--    os \<- many1 (char \'o\')   \-\-Applies the parser \"char \'o\'\" one or more times
--    string \"rld\"             \-\-Parses the string \"rld\"
--    return (length os)       \-\-Return the number of o\'s parsed
-- @

-- $quickstart3
-- 
-- This parser will turn a string of comma separated values into a list of 
-- them. Note that we could use the 'sepBy' parser, but this demonstrates a 
-- recursive parser.
--
-- @
--csv = do  
--    v \<- many (noneOf \",\")                \-\-Parses as many non-comma characters as possible
--    vs \<- option [] (char \',\' >> csv)     \-\-Optionally parses a comma and a csv, returning the empty list upon failure
--    return (v:vs)                         \-\-Concatenates and returns the full list
-- @

-- | Supplies the input to the 'Parser'. Returns all 'Result' types, 
-- including 'Partial' results.
runParser :: Parser s a -> s -> [Result s a]
runParser p = parse p . Open

-- | This runs the 'Parser' greedily over the input, 'finalize's all the 
-- results, and returns the first successful result. If there are no 
-- successful results, it returns 'Nothing'. This is useful when you are
-- parsing something that you know will have no 'Partial's and you just
-- want an answer.
parseOnly :: Stream s t => Parser s a -> s -> Maybe a
parseOnly p s = 
    case finalize (parse (greedy p) (Open s)) of
        [] -> Nothing
        (Done v _:_) -> Just v
