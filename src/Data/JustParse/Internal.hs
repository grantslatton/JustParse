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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Safe #-}

module Data.JustParse.Internal (
      Stream (..)
    , Parser (..)
    , Result (..)
    , InternalStream (..)
    , isDone
    , isPartial
    , toPartial
    , finalize
    , extend
    , extend'
    , streamAppend
) where

import Prelude hiding ( length )
import Control.Monad ( MonadPlus, mzero, mplus, (>=>), ap )
import Control.Applicative ( Alternative, Applicative, pure, (<*>), empty, (<|>) )
import Data.Monoid ( Monoid, mempty, mappend )

-- | A @Stream@ instance has a stream of type @s@, made up of tokens of 
-- type @t@, which must be determinable by the stream. A minimal complete
-- definition only needs to define @uncons@.
class (Eq s, Monoid s) => Stream s t | s -> t where
    -- | @uncons@ returns 'Nothing' if the @Stream@ is empty, otherwise it
    -- returns the first token of the stream, followed by the remainder
    -- of the stream, wrapped in a 'Just'.
    uncons :: Stream s t => s -> Maybe (t, s)
    -- | The default @length@ implementation is @O(n * O(uncons))@. If your stream 
    -- provides a more efficient method for determining the length, it is 
    -- wise to override this. The @length@ method is only used by the 
    -- 'greedy' parser.
    length :: Stream s t => s -> Int
    length s = case uncons s of
        Nothing -> 0
        Just (_, xs) -> 1 + length xs

data InternalStream s = 
      Open s
    | Closed s
    | Null

newtype Parser s a = Parser { parse :: InternalStream s -> [Result s a] }

instance Stream s t => Monoid (Parser s a) where
    mempty = mzero
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

instance Functor (Parser s) where
    fmap f (Parser p) = Parser $ map (fmap f) . p 
    {-# INLINE fmap #-}

instance Applicative (Parser s) where
    pure = return 
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Stream s t => Alternative (Parser s) where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Monad (Parser s) where
    return v = Parser $ \s -> [Done v s] 
    {-# INLINE return #-}
    (Parser p) >>= f = Parser $ p >=> g
        where
            g (Done a s) = parse (f a) s 
            g (Partial p') = [Partial $ p' >=> g] 
    {-# INLINE (>>=) #-}

instance Stream s t => MonadPlus (Parser s) where
    mzero = Parser $ const []
    {-# INLINE mzero #-}
    mplus a b = Parser $ \s -> let
        g [] = parse b s
        g xs 
            | any isDone xs = xs
            | otherwise = [Partial $ \s' -> case s' of
                Null -> case finalize (parse a s) of 
                    [] -> finalize (parse b s)
                    r -> r
                _ -> parse (mplus a b) (streamAppend s s')]
        in
            g (parse a s) 
    {-# INLINE mplus #-}

data Result s a =
      Partial { continue :: InternalStream s -> [Result s a] }
    | Done { value :: a, leftover :: InternalStream s }


isDone :: Result s a -> Bool
isDone (Done _ _) = True
isDone _ = False
{-# INLINE isDone #-}

isPartial :: Result s a -> Bool
isPartial (Partial _) = True
isPartial _ = False
{-# INLINE isPartial #-}

-- | Lifts a parser into the result space
toPartial :: Parser s a -> [Result s a]
toPartial (Parser p) = [Partial p]
{-# INLINE toPartial #-}

instance Functor (Result s) where
    fmap f (Partial p) = Partial $ map (fmap f) . p
    fmap f (Done a s) = Done (f a) s
    {-# INLINE fmap #-}

finalize :: Stream s t => [Result s a] -> [Result s a]
finalize rs = rs >>= g 
    where
        g (Partial p) = p Null
        g (Done a s') = [Done a (streamAppend s' Null)]
{-# INLINE finalize #-}

-- | Supplies partial results with the given 'Stream'
extend :: Stream s t =>  [Result s a] -> s -> [Result s a]
extend p = extend' p . Open
{-# INLINE extend #-}

extend' :: Stream s t => [Result s a] -> InternalStream s -> [Result s a]
extend' rs s = rs >>= g 
    where
        g (Partial p) = p s
        g (Done a s') = [Done a (streamAppend s' s)]
{-# INLINE extend' #-}

streamAppend :: Stream s t => InternalStream s -> InternalStream s -> InternalStream s
streamAppend Null _ = Null
streamAppend (Open s) Null = Closed s
streamAppend (Open s) (Open s') = Open (mappend s s')
streamAppend (Open s) (Closed s') = Closed (mappend s s')
streamAppend (Closed s) _ = Closed s
{-# INLINE streamAppend #-}
