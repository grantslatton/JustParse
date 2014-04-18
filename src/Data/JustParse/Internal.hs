{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.JustParse.Internal (
    satisfy,
    mN,
    finalize,
    extend,
    Stream (..),
    Parser (..),
    Result (..),
    isDone,
    isFail,
    isPartial,
    rename,
    (<?>)
) where

import Prelude hiding ( length )
import Control.Monad ( MonadPlus, mzero, mplus, (>=>), ap )
import Control.Applicative ( Alternative, Applicative, pure, (<*>), empty, (<|>) )
import Data.Monoid ( Monoid, mempty, mappend )

-- Typical uncons method as found in a lot of containers such as ByteString
-- Stream type s, token type t. Monoid s so we have access to mempty. Eq s
-- so we can see if s is mempty. 
class (Eq s, Monoid s) => Stream s t | s -> t where
    uncons :: Stream s t => s -> Maybe (t, s)
    length :: Stream s t => s -> Int
    length s = 
        case uncons s of
            Nothing -> 0
            Just (x, xs) -> 1 + length xs


-- A Parser is a function with Stream type s t. Takes a stream, returns all
-- successful or partial parses.
data Parser s t a = 
    Parser { 
        parse :: Stream s t => Maybe s -> [Result s t a]
    }

instance Stream s t => Monoid (Parser s t a) where
    mempty = mzero
    mappend = mplus

-- fmap f on a parser returns a parser that maps (fmap f) over its future
-- results.
instance Stream s t => Functor (Parser s t) where
    fmap f (Parser p) = Parser $ \s -> map (fmap f) (p s)

-- Uses the fact that Parser s t is a Monad
instance Stream s t => Applicative (Parser s t) where
    pure = return 
    (<*>) = ap

-- The alternative of two parsers is the concatentation of their results on
-- the same input
instance Stream s t => Alternative (Parser s t) where
    empty = mzero
    (<|>) = mplus

-- Returning a value returns a Parser that takes a Stream, and returns a 
-- Done containing the value, and the Stream as a leftover
instance Stream s t => Monad (Parser s t) where
    return v = Parser $ \s -> [Done v s] 
    (Parser p) >>= f = Parser $ p >=> g
        where
            g (Fail m l) = [Fail m l]
            g (Done a s) = parse (f a) s 
            g (Partial p) = [Partial $ p >=> g] 


-- mzero always returns no results
-- mplus is the same as alternative
instance Stream s t => MonadPlus (Parser s t) where
    mzero = Parser $ const []
    mplus (Parser p1) (Parser p2) = Parser (\s -> p1 s ++ p2 s)

data Result s t a =
    Partial {
        continue    :: Stream s t => Maybe s -> [Result s t a]
    } |
    Done {
        value       :: a,
        leftover    :: Maybe s
    } |
    Fail {
        message     :: String,
        leftover    :: Maybe s
    }

isDone :: Result s t a -> Bool
isDone (Done _ _) = True
isDone _ = False

isPartial :: Result s t a -> Bool
isPartial (Partial _) = True
isPartial _ = False

isFail :: Result s t a -> Bool
isFail (Fail _ _) = True
isFail _ = False

-- fmap f on a Done merely applys the function to the value inside
-- fmap f on a partial returns another partial that will map fmap f
-- on its results
instance Stream s t => Functor (Result s t) where
    fmap f (Partial p) = Partial $ map (fmap f) . p
    fmap f (Done a s) = Done (f a) s
    fmap f (Fail m l) = Fail m l

instance (Show a, Stream s t) => Show (Result s t a) where
    show (Partial _) = "Partial"
    show (Done a _) = show a
    show (Fail m l) = "Fail: " ++ m

-- Takes a condition and returns a Parser that, upon receiving input,
-- uncons the input. If the uncons fails, it returns a Partial that,
-- upon receiving input, uncons the input. If that fails, it returns
-- the empty list. If it succeeds, or if the original uncons succeeds,
-- if the condition is true for the uncons'd token, the token is 
-- returned wrapped in a Done, otherwise, the empty list is returned
satisfy :: Stream s t => (t -> Bool) -> Parser s t t
satisfy f = Parser $ \s -> 
    case s of
        Nothing -> [Fail "satisfy" s]
        Just s' -> case uncons s' of
            Nothing -> [Partial $ parse (satisfy f)]
            Just (x, xs) -> 
                if f x 
                    then [Done x (Just xs)]
                    else [Fail "satisfy" s]

-- Takes a lower and upper bound, and a Parser. Returns all of the
-- results of the parser being apply M through N times. To make N
-- infinity, simply supply a negative upper bound.
mN :: Stream s t => Int -> Int -> Parser s t a -> Parser s t [a]
mN _ 0 _ = Parser $ \s -> [Done [] s] 
mN m n p = Parser $ \s -> 
    if m == 0 
        then Done [] s : (parse p s >>= g)
        else             parse p s >>= g
    where
        m' = if m == 0 then 0 else m-1
        g (Done a s) = parse (mN m' (n-1) p) s >>= h a
        g (Partial p') = [Partial $ p' >=> g]
        g (Fail m l) = [Fail "mN" l]
        h a (Done as s) = [Done (a:as) s]
        h a (Partial p') = [Partial $ p' >=> h a]
        h a (Fail m l) = [Fail "mN" l]

-- Takes results and supplies mempty to any remaining Partials
finalize :: Stream s t => [Result s t a] -> [Result s t a]
finalize = extend Nothing

-- Takes results and input and supplies the input to any Partials
extend :: Stream s t => Maybe s -> [Result s t a] -> [Result s t a]
extend s rs = rs >>= g --`prnt` (show (map i rs, map i (rs >>= g), h s))
    where
        g (Fail m l) = [Fail m (f l s)]
        g (Partial p) = p s
        g (Done a s') = [Done a (f s' s)]
        f Nothing _ = Nothing
        f (Just s) Nothing = if s == mempty then Nothing else Just s
        f s s' = mappend s s'

rename :: Stream s t => String -> Parser s t a -> Parser s t a
rename s p = Parser (map g . parse p)
    where
        g v@(Done _ _) = v
        g v@(Fail _ l) = Fail s l
        g v@(Partial _) = v

infix 0 <?>
(<?>) :: Stream s t => Parser s t a -> String -> Parser s t a
p <?> s = rename s p
