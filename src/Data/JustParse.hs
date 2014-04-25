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

    -- * General Parsing
    C.Stream(..),
    C.Result(..),
    C.Parser( parse ),
    C.justParse,
    C.runParser,
    C.finalize,
    C.extend,
    C.isDone,
    C.isFail,
    C.isPartial,
    C.rename,
    (C.<?>),

    -- * Generic Parsers
    C.test,
    C.greedy,
    C.option,
    C.satisfy,
    C.mN,
    C.many,
    C.many1,
    C.manyN,
    C.atLeast,
    C.exactly,
    C.eof,
    C.oneOf,
    C.noneOf,
    C.token,
    C.anyToken,
    C.lookAhead,

    -- * Char Parsers
    C.char,
    C.anyChar,
    C.ascii,
    C.latin1,
    C.control,
    C.space,
    C.lower,
    C.upper,
    C.alpha,
    C.alphaNum,
    C.print,
    C.digit,
    C.octDigit,
    C.hexDigit,
    C.eol,

    -- * String Parsers
    C.string,

    -- * Regex Parsers
    L.regex,
    L.regex',
    L.Match(..)

) where

import Data.JustParse.Language as L
import Data.JustParse.Common as C

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
