{-|
Module      : Data.JustParse
Description : The one-stop import for the library
Copyright   : Copyright Waived
License     : PublicDomain
Maintainer  : grantslatton@gmail.com
Stability   : experimental
Portability : portable

A simple and comprehensive Haskell parsing library.
-} 

--{-# LANGUAGE Safe #-}
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
    parseOnly
) where

import Data.JustParse.Internal
import Data.JustParse.Combinator
import Data.JustParse.Prim

-- $overview
-- 
-- * Allows for parsing arbitrary 'Stream' types
-- 
-- * Makes extensive use of combinators
-- 
-- * Returns relatively verbose 'Fail'ure messages.
-- 
-- * Allows one to 'rename' a 'Parser'. 
-- 
-- * Allows for a parser to return a 'Partial' 'Result'
-- 
-- * Non-greedy parsing
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
--    exp \<- char \'!\'                 \-\-Parses the character \'!\'
--    return ([h]++rest++[exp])       \-\-Returns all of the above concatenated together
-- @

-- $quickstart2
--
-- This parser will accept the string @\"hello woooooorld\"@ with any number of @o@\'s.
-- It returns the number of @o@\'s.
--
-- @
--p = do
--    first \<- string \"hello w\"         \-\-Parses the string \"hello w\"
--    os \<- many1 (char \'o\')            \-\-Applies the parser \"char \'o\'\" one or more times
--    second \<- string \"rld\"            \-\-Parses the string \"rld\"
--    return (length os)                \-\-Return the number of o\'s parsed
-- @

-- $quickstart3
-- 
-- This parser will turn a string of comma separated values into a list of them
--
-- @
--csv = do  
--    v \<- greedy (many (noneOf \",\"))       \-\-Parses as many non-comma characters as possible
--    vs \<- option [] (char \',\' >> csv)     \-\-Optionally parses a comma and a csv, returning the empty list upon failure
--    return (v:vs)                         \-\-Concatenates and returns the full list
-- @


-- | Supplies the input to the 'Parser'. Returns all 'Result' types, 
-- including 'Partial' results.
runParser :: Parser s a -> s -> [Result s a]
runParser p = parse p . Just

-- This runs the 'Parser' greedily over the input, 'finalize's all the 
-- results, and returns the first successful result. If there are no 
-- successful results, it returns Nothing. This is useful when you are
-- parsing something that you know will have no 'Partial's and you just
-- want an answer.
parseOnly :: Stream s t => Parser s a -> s -> Maybe a
parseOnly p s = 
    case finalize (parse (greedy p) (Just s)) of
        [] -> Nothing
        (Done v _:_) -> Just v

