{-|
Module      : Data.JustParse.Common
Description : Common Parser Combinators
Copyright   : Copyright Waived
License     : PublicDomain
Maintainer  : grantslatton@gmail.com
Stability   : experimental
Portability : portable

Many common parsing needs in one place.
-}

--{-# LANGUAGE Safe #-}
module Data.JustParse.Combinator (
    test,
    greedy,
    option,
    optional,
    optional_,
    (<|>),
    choice,
    choice_,
    fork,
    (<||>),
    mN,
    mN_,
    many,
    many_,
    many1,
    many1_,
    sepBy,
    sepBy_,
    sepBy1,
    sepBy1_,
    lookAhead,
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
import qualified Control.Applicative as A

-- | Splits the current parse branch between the two parsers.
fork :: Parser s a -> Parser s a -> Parser s a
fork a b = Parser $ \s -> parse a s ++ parse b s

infixr 1 <||>
-- | Infix version of 'fork'.
(<||>) :: Parser s a -> Parser s a -> Parser s a
(<||>) = fork

-- | Applies the given parser from @m@ to @n@ times, inclusive.
mN :: (Eq s, Monoid s) => Int -> Int -> Parser s a -> Parser s [a]
mN _ 0 _ = Parser $ \s -> [Done [] s]
mN 0 n p = liftM2 (:) p (mN 0 (n-1) p) A.<|> return []
mN m n p = liftM2 (:) p (mN (m-1) (n-1) p)

-- | Splits off a new branch for every possible value between @m@ and @n@,
-- rather than parsing as many as possible.
mN_ :: (Eq s, Monoid s) => Int -> Int -> Parser s a -> Parser s [a]
mN_ _ 0 _ = Parser $ \s -> [Done [] s]
mN_ 0 n p = liftM2 (:) p (mN 0 (n-1) p) <||> return []
mN_ m n p = liftM2 (:) p (mN (m-1) (n-1) p)

-- | Applies the parser as many times as possible, returning a list of
-- results. It can potentially return an empty list upon zero successful 
-- parses.
many :: (Eq s, Monoid s) => Parser s a -> Parser s [a]
many = A.many

-- | Splits off a new branch for every possible number of applications, 
-- rather than parsing as many as possible.
many_ :: Parser s a -> Parser s [a]
many_ p = return [] <||> liftM2 (:) p (many_ p)

-- | Applies the parser as many times as possible, returning a list of 
-- results. At least one successful parse must be found.
many1 :: (Eq s, Monoid s) => Parser s a -> Parser s [a]
many1 p = liftM2 (:) p (many p)

-- | Splits off a new branch for every possible number of applications,
-- rather than parsing as many as possible. At least one successful parse
-- must be found.
many1_ :: Parser s a -> Parser s [a]
many1_ p = liftM2 (:) p (many_ p)

-- | Return @True@ if the parser would succeed if one were to apply it,
-- otherwise, it returns@False@. It does not consume input.
test :: (Eq s, Monoid s) => Parser s a -> Parser s Bool
test p = 
    do 
        a <- optional (lookAhead p)
        case a of
            Nothing -> return False
            _ -> return True

(<|>) :: (Eq s, Monoid s) => Parser s a -> Parser s a -> Parser s a
(<|>) = (A.<|>)

-- | Given a list of parsers, try each one in order until one succeeds, and
-- return its results. Fail if no parsers succeed.
choice :: (Eq s, Monoid s) => [Parser s a] -> Parser s a
choice = foldl1' (A.<|>) 

-- | Given a list of parsers, split off a branch for each one.
choice_ :: (Eq s, Monoid s) => [Parser s a] -> Parser s a
choice_ = foldl1' (<||>)

-- | Modifies a 'Parser' so that it will ony return the most consumptive
-- succesful results. 
greedy :: Stream s t => Parser s a -> Parser s a
greedy (Parser p) = Parser $ \s -> g (p s) 
    where
        f Nothing = 0
        f (Just s) = length s
        g [] = []
        g xs 
            | all isDone xs = [minimumBy (comparing (f . leftover)) xs]
            | otherwise = [Partial $ \s -> g $ extend s xs] 

-- | Attempts to apply a parser and returns a default value if it fails.
option :: (Eq s, Monoid s) => a -> Parser s a -> Parser s a
option v p = 
    do
        r <- A.optional p
        case r of
            Nothing -> return v
            Just v' -> return v'

-- | Attempts to apply the parser, returning 'Nothing' upon failure, or
-- the result wrapped in a 'Just'.
optional :: (Eq s, Monoid s) => Parser s a -> Parser s (Maybe a)
optional = A.optional

-- | Splits off two branches, one where the parse is attempted, and one 
-- where it is not.
optional_ :: (Eq s, Monoid s) => Parser s a -> Parser s (Maybe a)
optional_ p = liftM Just p <||> return Nothing

-- | @sepBy p s@ parses any number of occurences of @p@ separated by @s@.
-- Returns a list of @p@'s results. The results of @s@ are discarded. 
-- It can potentially return an empty list upon zero successful 
-- parses.
sepBy :: (Eq s, Monoid s) => Parser s a -> Parser s b -> Parser s [a]
sepBy p s = sepBy1 p s A.<|> return []

-- | @sepBy_ p s@ splits off a new branch for every possible number of
-- applications of @p@ separated by @s@.
sepBy_ :: (Eq s, Monoid s) => Parser s a -> Parser s b -> Parser s [a]
sepBy_ p s = sepBy1_ p s <||> return []

-- | @sepBy1 p s@ parses any number of occurences of @p@ separated by @s@.
-- Returns a list of @p@'s results. The results of @s@ are discarded. 
-- At least one successful parse must be found.
sepBy1 :: (Eq s, Monoid s) => Parser s a -> Parser s b -> Parser s [a]
sepBy1 p s = liftM2 (:) p (many (s >> p))

-- | @sepBy1_ p s@ splits off a new branch for every possible number of 
-- applications of @p@ separated by @s@. At least one successfull parse 
-- must be found.
sepBy1_ :: (Eq s, Monoid s) => Parser s a -> Parser s b -> Parser s [a]
sepBy1_ p s = liftM2 (:) p (many_ (s >> p))

-- | Applies the parser and returns its result, but resets
-- the leftovers as if it consumed nothing.
lookAhead :: (Eq s, Monoid s) => Parser s a -> Parser s a
lookAhead v@(Parser p) = Parser $ \s -> 
    let 
        g (Done a _) = Done a s
        g (Partial p') = Partial $ \s' -> 
            case s' of
                Nothing -> finalize (p' s)
                _ -> parse (lookAhead v) (streamAppend s s')
    in
        map g (p s)