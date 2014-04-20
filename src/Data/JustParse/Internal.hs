{-|
Module      : Data.JustParse.Internal
Description : The engine behind the JustParse library
Copyright   : Copyright Waived
License     : PublicDomain
Maintainer  : grantslatton@gmail.com
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Safe #-}

module Data.JustParse.Internal (
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
import Data.List ( intercalate )

-- | A @Stream@ instance has a stream of type @s@, made up of tokens of 
-- type @t@, which must be determinable by the stream.
class (Eq s, Monoid s) => Stream s t | s -> t where
    -- | @uncons@ returns @Nothing@ if the @Stream@ is empty, otherwise it
    -- returns the first token of the stream, followed by the remainder
    -- of the stream, wrapped in a @Just@.
    uncons :: Stream s t => s -> Maybe (t, s)
    -- | The default @length@ implementation is O(n). If your stream provides
    -- a more efficient method for determining the length, it is wise to
    -- override this. The @length@ method is only used by the 'greedy' parser.
    length :: Stream s t => s -> Int
    length s = 
        case uncons s of
            Nothing -> 0
            Just (x, xs) -> 1 + length xs

newtype Parser s a = 
    Parser { 
        parse :: Maybe s -> [Result s a]
    }

instance Monoid (Parser s a) where
    mempty = mzero
    mappend = mplus

instance Functor (Parser s) where
    fmap f (Parser p) = Parser $ \s -> map (fmap f) (p s)

instance Applicative (Parser s) where
    pure = return 
    (<*>) = ap

instance Alternative (Parser s) where
    empty = mzero
    (<|>) = mplus

instance Monad (Parser s) where
    return v = Parser $ \s -> [Done v s] 
    (Parser p) >>= f = Parser $ p >=> g
        where
            g (Fail m l) = [Fail m l]
            g (Done a s) = parse (f a) s 
            g (Partial p) = [Partial $ p >=> g] 

instance MonadPlus (Parser s) where
    mzero = Parser $ const []
    mplus (Parser p1) (Parser p2) = Parser (\s -> p1 s ++ p2 s)

data Result s a 
    -- | A @Partial@ wraps the same function as a Parser. Supply it with a @Just@
    -- and it will continue parsing, or with a @Nothing@ and it will terminate.
    =
    Partial {
        continue    :: Maybe s -> [Result s a]
    } |
    -- | A @Done@ contains the resultant @value@, and the @leftover@ stream, if any.
    Done {
        value       :: a,
        leftover    :: Maybe s
    } |
    -- | A @Fail@ contains a stack of error messages, and the @lftover@ stream, if any.
    Fail {
        messages    :: [String],
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

instance Functor (Result s) where
    fmap f (Partial p) = Partial $ map (fmap f) . p
    fmap f (Done a s) = Done (f a) s
    fmap f (Fail m l) = Fail m l

instance Show a => Show (Result s a) where
    show (Partial _) = "Partial"
    show (Done a _) = show a
    show (Fail m l) = "Fail: \nIn: " ++ intercalate "\nIn: " m

-- | @finalize@ takes a list of results (presumably returned from a 'Parser' or 'Partial',
-- and supplies @Nothing@ to any remaining @Partial@ values, so that only 'Fail' and 'Done'
-- values remain.
finalize :: (Eq s, Monoid s) => [Result s a] -> [Result s a]
finalize = extend Nothing

-- | @extend@ takes a @Maybe s@ as input, and supplies the input to all values
-- in the 'Result' list. For 'Done' and 'Fail' values, it appends the @stream@ 
-- to the 'leftover' portion, and for 'Partial' values, it runs the continuation,
-- adding in any new 'Result' values to the output.
extend :: (Eq s, Monoid s) => Maybe s -> [Result s a] -> [Result s a]
extend s rs = rs >>= g --`prnt` (show (map i rs, map i (rs >>= g), h s))
    where
        g (Fail m l) = [Fail m (f l s)]
        g (Partial p) = p s
        g (Done a s') = [Done a (f s' s)]
        f Nothing _ = Nothing
        f (Just s) Nothing = if s == mempty then Nothing else Just s
        f s s' = mappend s s'

-- | @rename@ pushes a new error message onto the stack in case of failure.
-- This is particularly useful when debugging a complex 'Parser'.
rename :: String -> Parser s a -> Parser s a
rename s p = Parser (map g . parse p)
    where
        g v@(Fail m l) = Fail (s:m) l
        g v = v


infixl 0 <?>
-- | The infix version of 'rename'
(<?>) :: Parser s a -> String -> Parser s a
p <?> s = rename s p
