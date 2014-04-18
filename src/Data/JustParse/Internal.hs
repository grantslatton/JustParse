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


-- A Parser is a function with Stream type s. Takes a stream, returns all
-- successful, partial, or failed parses.
data Parser s a = 
    Parser { 
        parse :: Maybe s -> [Result s a]
    }

instance Monoid (Parser s a) where
    mempty = mzero
    mappend = mplus

-- fmap f on a parser returns a parser that maps (fmap f) over its future
-- results.
instance Functor (Parser s) where
    fmap f (Parser p) = Parser $ \s -> map (fmap f) (p s)

-- Uses the fact that Parser s is a Monad
instance Applicative (Parser s) where
    pure = return 
    (<*>) = ap

-- The alternative of two parsers is the concatentation of their results on
-- the same input
instance Alternative (Parser s) where
    empty = mzero
    (<|>) = mplus

-- Returning a value returns a Parser that takes a Stream, and returns a 
-- Done containing the value, and the Stream as a leftover
-- Bind returns a parser than, upon getting results, pipes the leftovers
-- of those results into the next parser. 
instance Monad (Parser s) where
    return v = Parser $ \s -> [Done v s] 
    (Parser p) >>= f = Parser $ p >=> g
        where
            g (Fail m l) = [Fail m l]
            g (Done a s) = parse (f a) s 
            g (Partial p) = [Partial $ p >=> g] 

-- mzero always returns no results
-- mplus runs both parsers and concatenates their results.
instance MonadPlus (Parser s) where
    mzero = Parser $ const []
    mplus (Parser p1) (Parser p2) = Parser (\s -> p1 s ++ p2 s)

data Result s a =
    Partial {
        continue    :: Maybe s -> [Result s a]
    } |
    Done {
        value       :: a,
        leftover    :: Maybe s
    } |
    Fail {
        message     :: String,
        leftover    :: Maybe s
    }

isDone :: Result s a -> Bool
isDone (Done _ _) = True
isDone _ = False

isPartial :: Result s a -> Bool
isPartial (Partial _) = True
isPartial _ = False

isFail :: Result s a -> Bool
isFail (Fail _ _) = True
isFail _ = False

-- fmap f on a Done merely applys the function to the value inside
-- fmap f on a partial returns another partial that will map fmap f
-- on its results
instance Functor (Result s) where
    fmap f (Partial p) = Partial $ map (fmap f) . p
    fmap f (Done a s) = Done (f a) s
    fmap f (Fail m l) = Fail m l

instance Show a => Show (Result s a) where
    show (Partial _) = "Partial"
    show (Done a _) = show a
    show (Fail m l) = "Fail: " ++ m

-- Takes a condition and returns a Parser that, upon receiving input,
-- Fails if the input is Nothing, otherwise it uncons the input and
-- returns a partial if the input is empty and tries to satisfy the
-- function otherwise.
satisfy :: Stream s t => (t -> Bool) -> Parser s t
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
mN :: Int -> Int -> Parser s a -> Parser s [a]
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
finalize :: (Eq s, Monoid s) => [Result s a] -> [Result s a]
finalize = extend Nothing

-- Takes results and input and supplies the input to any Partials
extend :: (Eq s, Monoid s) => Maybe s -> [Result s a] -> [Result s a]
extend s rs = rs >>= g --`prnt` (show (map i rs, map i (rs >>= g), h s))
    where
        g (Fail m l) = [Fail m (f l s)]
        g (Partial p) = p s
        g (Done a s') = [Done a (f s' s)]
        f Nothing _ = Nothing
        f (Just s) Nothing = if s == mempty then Nothing else Just s
        f s s' = mappend s s'

rename :: String -> Parser s a -> Parser s a
rename s p = Parser (map g . parse p)
    where
        g v@(Fail _ l) = Fail s l
        g v = v

infixl 0 <?>
(<?>) :: Parser s a -> String -> Parser s a
p <?> s = rename s p
