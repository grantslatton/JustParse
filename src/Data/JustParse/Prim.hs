{-|
Module      : Data.JustParse.Common
Description : Primitive Parsers
Copyright   : Copyright Waived
License     : PublicDomain
Maintainer  : grantslatton@gmail.com
Stability   : experimental
Portability : portable

Many common parsing needs in one place.
-}

--{-# LANGUAGE Safe #-}
module Data.JustParse.Prim (
    Parser, 
    Stream,
    satisfy,
    assert,
    eof,
    oneOf,
    noneOf,
    token,
    anyToken
) where

import Prelude hiding ( print, length )
import Data.JustParse.Internal ( 
    Stream(..), Parser(..), Result(..), extend, finalize, isDone, 
    isPartial, toPartial, streamAppend )
import Data.Monoid ( mempty, Monoid, mappend )
import Data.Maybe ( fromMaybe )
import Data.List ( minimumBy, foldl1' )
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

-- | A parser that succeeds on True and fails on False.
assert :: (Eq s, Monoid s) => Bool -> Parser s ()
assert True = return ()
assert False = mzero

-- | Only succeeds when supplied with @Nothing@.
eof :: (Eq s, Monoid s) => Parser s ()
eof = Parser $ \s ->
    case s of
        Nothing -> [Done () s]
        Just s' -> [Partial $ parse eof | s' == mempty]

-- | Parse a token that is a member of the list of tokens.
oneOf :: (Eq t, Stream s t) => [t] -> Parser s t
oneOf ts = satisfy (`elem` ts)

-- | Parse a toekn that is not a member of the list of tokens.
noneOf :: (Eq t, Stream s t) => [t] -> Parser s t
noneOf ts = satisfy (`notElem` ts)

-- | Parse a specific token.
token :: (Eq t, Stream s t) => t -> Parser s t
token t = satisfy (==t)

-- | Parse any token.
anyToken :: Stream s t => Parser s t
anyToken = satisfy (const True)
