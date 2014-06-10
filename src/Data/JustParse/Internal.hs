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
--{-# LANGUAGE Safe #-}

module Data.JustParse.Internal (
    Stream (..),
    Parser (..),
    Result (..),
    isDone,
    isPartial,
    toPartial,
    finalize,
    extend,
    streamAppend
) where

import Prelude hiding ( length )
import Control.Monad ( MonadPlus, mzero, mplus, (>=>), ap )
import Control.Applicative ( Alternative, Applicative, pure, (<*>), empty, (<|>) )
import Data.Monoid ( Monoid, mempty, mappend )
import Data.List ( intercalate )

-- | A @Stream@ instance has a stream of type @s@, made up of tokens of 
-- type @t@, which must be determinable by the stream.
class (Eq s, Monoid s) => Stream s t | s -> t where
    -- | @uncons@ returns 'Nothing' if the @Stream@ is empty, otherwise it
    -- returns the first token of the stream, followed by the remainder
    -- of the stream, wrapped in a 'Just'.
    uncons :: Stream s t => s -> Maybe (t, s)
    -- | The default @length@ implementation is @O(n)@. If your stream 
    -- provides a more efficient method for determining the length, it is 
    -- wise to override this. The @length@ method is only used by the 
    -- 'greedy' parser.
    length :: Stream s t => s -> Int
    length s = 
        case uncons s of
            Nothing -> 0
            Just (x, xs) -> 1 + length xs

newtype Parser s a = 
    Parser { 
        parse :: Maybe s -> [Result s a]
    }

instance (Eq s, Monoid s) => Monoid (Parser s a) where
    mempty = mzero
    mappend = mplus

instance Functor (Parser s) where
    fmap f (Parser p) = Parser $ map (fmap f) . p 

instance Applicative (Parser s) where
    pure = return 
    (<*>) = ap

instance (Eq s, Monoid s) => Alternative (Parser s) where
    empty = mzero
    (<|>) = mplus

instance Monad (Parser s) where
    return v = Parser $ \s -> [Done v s] 
    (Parser p) >>= f = Parser $ p >=> g
        where
            g (Done a s) = parse (f a) s 
            g (Partial p) = [Partial $ p >=> g] 

instance (Monoid s, Eq s) => MonadPlus (Parser s) where
    mzero = Parser $ const []
    mplus a b = Parser $ \s ->
        let
            g [] = parse b s
            g xs 
                | any isDone xs = xs
                | otherwise = [Partial $ \s' -> 
                    -- case needed for proper finalization
                    case s' of
                        -- if finalized
                        Nothing -> 
                            case finalize (parse a s) of
                                -- if parser a doesn't yield, try b
                                [] -> finalize (parse b s)
                                -- otherwise give what a yielded
                                r -> r
                        -- if not finalized
                        _ -> parse (mplus a b) (streamAppend s s')]

        in
            g (parse a s) 

data Result s a 
    -- | A @Partial@ wraps the same function as a Parser. Supply it with 
    -- a 'Just'
    -- and it will continue parsing, or with a 'Nothing' and it will 
    -- terminate.
    =
    Partial {
        continue    :: Maybe s -> [Result s a]
    } |
    -- | A @Done@ contains the resultant @value@, and the @leftover@ 
    -- stream, if any.
    Done {
        value       :: a,
        leftover    :: Maybe s
    } 

isDone :: Result s a -> Bool
isDone (Done _ _) = True
isDone _ = False

isPartial :: Result s a -> Bool
isPartial (Partial _) = True
isPartial _ = False

-- | Lifts a parser into the result space
toPartial :: Parser s a -> [Result s a]
toPartial (Parser p) = [Partial p]

instance Functor (Result s) where
    fmap f (Partial p) = Partial $ map (fmap f) . p
    fmap f (Done a s) = Done (f a) s

instance Show a => Show (Result s a) where
    show (Partial _) = "Partial"
    show (Done a _) = show a

-- | @finalize@ takes a list of results (presumably returned from a 
-- 'Parser' or 'Partial', and supplies 'Nothing' to any remaining 'Partial' 
-- values, so that only 'Done' values remain.
finalize :: (Eq s, Monoid s) => [Result s a] -> [Result s a]
finalize = extend Nothing

-- | @extend@ takes a @'Maybe' s@ as input, and supplies the input to all 
-- values  in the 'Result' list. For 'Done' values, it appends 
-- the 'Stream'  to the 'leftover' portion, and for 'Partial' values, it 
-- runs the continuation, adding in any new 'Result' values to the output.
extend :: (Eq s, Monoid s) => Maybe s -> [Result s a] -> [Result s a]
extend s rs = rs >>= g 
    where
        g (Partial p) = p s
        g (Done a s') = [Done a (streamAppend s' s)]

streamAppend :: (Eq s, Monoid s) => Maybe s -> Maybe s -> Maybe s
streamAppend Nothing _ = Nothing 
streamAppend (Just s) Nothing = if s == mempty then Nothing else Just s 
streamAppend s s' = mappend s s'
