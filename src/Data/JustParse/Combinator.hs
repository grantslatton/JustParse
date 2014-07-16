{-|
Module      : Data.JustParse.Common
Description : Common parser combinators
Copyright   : Copyright Waived
License     : PublicDomain
Maintainer  : grantslatton@gmail.com
Stability   : experimental
Portability : portable

The bread and butter of combinatory parsing.
-}

{-# LANGUAGE Safe #-}
module Data.JustParse.Combinator (
    -- * Utility Parsers
    assert,
    eof,
    eitherP,
    greedy,
    guard,
    lookAhead,
    notFollowedBy,
    option,
    optional,
    test,
    try,
    (<|>),

    -- * Token Parsers
    anyToken,
    noneOf,
    oneOf,
    satisfy,
    token,

    -- * Repetetive Parsers
    chainl,
    chainl1,
    chainr,
    chainr1,
    count,
    endBy,
    endBy1,
    exactly,
    many,
    many1,
    manyTill,
    mN,
    sepBy,
    sepBy1,
    skipMany,
    skipMany1,
    sepEndBy,
    sepEndBy1,
    takeWhile,
    takeWhile1,

    -- * Group Parsers
    choice,
    perm,
    select,

    -- * Branching Parsers
    branch,
    (<||>),
    chainl_,
    chainr_,
    chainl1_,
    chainr1_,
    choice_,
    eitherP_,
    endBy_,
    endBy1_,
    many_,
    many1_,
    mN_,
    option_,
    optional_,
    perm_,
    select_,
    sepBy_,
    sepBy1_,
    sepEndBy_,
    sepEndBy1_,
    skipMany_,
    skipMany1_,
    takeWhile_,
    takeWhile1_
) where

import Prelude hiding ( print, length, takeWhile )
import Data.JustParse.Internal ( 
    Stream(..), Parser(..), Result(..), extend, finalize, isDone, 
    isPartial, toPartial, streamAppend )
import Data.Monoid ( mempty, Monoid, mappend )
import Data.Maybe ( fromMaybe )
import Data.List ( minimumBy, foldl1', foldl' )
import Data.Ord ( comparing )
import qualified Control.Monad as M
import qualified Control.Applicative as A

-- | Parse a token that satisfies a predicate.
satisfy :: Stream s t => (t -> Bool) -> Parser s t
satisfy f = Parser $ \s -> 
    case s of
        Nothing -> []
        Just s' -> case uncons s' of
            Nothing -> [Partial $ parse (satisfy f)]
            Just (x, xs) -> [Done x (Just xs) | f x]
{-# INLINE satisfy #-}

-- | A parser that succeeds on 'True' and fails on 'False'. 
guard :: Stream s t => Bool -> Parser s ()
guard = M.guard
{-# INLINE guard #-}

-- | Synonym of 'guard'.
assert :: Stream s t => Bool -> Parser s ()
assert = guard
{-# INLINE assert #-}

-- | Only succeeds when supplied with 'Nothing'.
eof :: Stream s t => Parser s ()
eof = notFollowedBy anyToken
{-# INLINE eof #-}

-- | Parse a token that is a member of the list of tokens.
oneOf :: (Eq t, Stream s t) => [t] -> Parser s t
oneOf ts = satisfy (`elem` ts)
{-# INLINE oneOf #-}

-- | Parse a token that is not a member of the list of tokens.
noneOf :: (Eq t, Stream s t) => [t] -> Parser s t
noneOf ts = satisfy (`notElem` ts)
{-# INLINE noneOf #-}

-- | Parse a specific token.
token :: (Eq t, Stream s t) => t -> Parser s t
token t = satisfy (==t)
{-# INLINE token #-}

-- | Parse any token.
anyToken :: Stream s t => Parser s t
anyToken = satisfy (const True)
{-# INLINE anyToken #-}

-- | Parse tokens while a predicate remains true.
takeWhile :: Stream s t => (t -> Bool) -> Parser s [t]
takeWhile = many . satisfy
{-# INLINE takeWhile #-}

-- | Parse one or more tokens while a predicate remains true.
takeWhile1 :: Stream s t => (t -> Bool) -> Parser s [t]
takeWhile1 = many1 . satisfy
{-# INLINE takeWhile1 #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
takeWhile_ :: Stream s t => (t -> Bool) -> Parser s [t]
takeWhile_ = many_ . satisfy
{-# INLINE takeWhile_ #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
takeWhile1_ :: Stream s t => (t -> Bool) -> Parser s [t]
takeWhile1_ = many1_ . satisfy
{-# INLINE takeWhile1_ #-}

-- | Splits the current parse branch between the two parsers.
branch :: Parser s a -> Parser s a -> Parser s a
branch a b = Parser $ \s -> parse a s ++ parse b s
{-# INLINE branch #-}

infixr 1 <||>
-- | Infix version of 'branch'.
(<||>) :: Parser s a -> Parser s a -> Parser s a
(<||>) = branch
{-# INLINE (<||>) #-}

-- | @mN m n p@ parses between @m@ and @n@ occurences of @p@, inclusive.
mN :: Stream s t => Int -> Int -> Parser s a -> Parser s [a]
mN _ 0 _ = Parser $ \s -> [Done [] s]
mN 0 n p = M.liftM2 (:) p (mN 0 (n-1) p) A.<|> return []
mN m n p = M.liftM2 (:) p (mN (m-1) (n-1) p)
{-# INLINE mN #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
mN_ :: Stream s t => Int -> Int -> Parser s a -> Parser s [a]
mN_ _ 0 _ = Parser $ \s -> [Done [] s]
mN_ 0 n p = M.liftM2 (:) p (mN 0 (n-1) p) <||> return []
mN_ m n p = M.liftM2 (:) p (mN (m-1) (n-1) p)
{-# INLINE mN_ #-}

-- | Synonym of 'count'.
exactly :: Stream s t => Int -> Parser s a -> Parser s [a]
exactly n = mN n n
{-# INLINE exactly #-}

-- | Applies a parser at least @n@ times.
atLeast :: Stream s t => Int -> Parser s a -> Parser s [a]
atLeast n = mN n (-1)
{-# INLINE atLeast #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
atLeast_ :: Stream s t => Int -> Parser s a -> Parser s [a]
atLeast_ n = mN_ n (-1)
{-# INLINE atLeast_ #-}

-- | Applies a parser at most @n@ times.
atMost :: Stream s t => Int -> Parser s a -> Parser s [a]
atMost  = mN 0
{-# INLINE atMost #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
atMost_ :: Stream s t => Int -> Parser s a -> Parser s [a]
atMost_ = mN 0
{-# INLINE atMost_ #-}

-- | Applies a parser zero or more times.
many :: Stream s t => Parser s a -> Parser s [a]
many = A.many
{-# INLINE many #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
many_ :: Parser s a -> Parser s [a]
many_ p = return [] <||> M.liftM2 (:) p (many_ p)
{-# INLINE many_ #-}

-- | Applies a parser one or more times.
many1 :: Stream s t => Parser s a -> Parser s [a]
many1 p = M.liftM2 (:) p (many p)
{-# INLINE many1 #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
many1_ :: Parser s a -> Parser s [a]
many1_ p = M.liftM2 (:) p (many_ p)
{-# INLINE many1_ #-}

-- | Return 'True' if the parser would succeed if one were to apply it,
-- otherwise, it returns 'False'. It does not consume input.
test :: Stream s t => Parser s a -> Parser s Bool
test p = 
    do 
        a <- optional (lookAhead p)
        case a of
            Nothing -> return False
            _ -> return True
{-# INLINE test #-}

infixr 1 <|>
-- | @a \<|\> b@ is equivalent to @'choice' [a,b]@. That is, first @a@ is
-- tried, and if it yields no results, @b@ is tried.
(<|>) :: Stream s t => Parser s a -> Parser s a -> Parser s a
(<|>) = (A.<|>)
{-# INLINE (<|>) #-}

-- | Attempt to apply each parser in the list in order until one succeeds.
choice :: Stream s t => [Parser s a] -> Parser s a
choice = foldl1' (A.<|>) 
{-# INLINE choice #-}

-- | Given a list of parsers, split off a branch for each one.
choice_ :: Stream s t => [Parser s a] -> Parser s a
choice_ = foldl1' (<||>)
{-# INLINE choice_ #-}

-- | Like 'choice', but returns the index of the successful parser as well
-- as the result.
select :: Stream s t => [Parser s a] -> Parser s (Int, a)
select [] = M.mzero
select (p:ps) = M.liftM (0,) p <|> M.liftM (\(x,y) -> (x+1,y)) (select ps)
{-# INLINE select #-}

-- | Like 'choice_', but returns the index of the successful parser.
select_ :: Stream s t => [Parser s a] -> Parser s (Int, a)
select_ [] = M.mzero
select_ (p:ps) = M.liftM (0,) p <||> M.liftM (\(x,y) -> (x+1,y)) (select_ ps)
{-# INLINE select_ #-}

-- | Modifies a parser so that it will ony return the most consumptive
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
{-# INLINE greedy #-}

-- | Attempts to apply a parser and returns a default value if it fails.
option :: Stream s t => a -> Parser s a -> Parser s a
option v p = 
    do
        r <- A.optional p
        case r of
            Nothing -> return v
            Just v' -> return v'
{-# INLINE option #-}

-- | Splits off two branches, one where the parse is attempted, and one 
-- where it is not.
option_ :: Stream s t => a -> Parser s a -> Parser s a
option_ v p = option v p <||> return v
{-# INLINE option_ #-}

-- | Attempts to apply the parser, returning 'Nothing' upon failure, or
-- the result wrapped in a 'Just'.
optional :: Stream s t => Parser s a -> Parser s (Maybe a)
optional = A.optional
{-# INLINE optional #-}

-- | Splits off two branches, one where the parse is attempted, and one 
-- where it is not.
optional_ :: Stream s t => Parser s a -> Parser s (Maybe a)
optional_ p = M.liftM Just p <||> return Nothing
{-# INLINE optional_ #-}

-- | @sepBy1 p s@ parses many  occurences of @p@ separated by @s@.
sepBy :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
sepBy p s = sepBy1 p s A.<|> return []
{-# INLINE sepBy #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
sepBy_ :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
sepBy_ p s = sepBy1_ p s <||> return []
{-# INLINE sepBy_ #-}

-- | @sepBy1 p s@ parses one or more occurences of @p@ separated by @s@.
sepBy1 :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
sepBy1 p s = M.liftM2 (:) p (many (s >> p))
{-# INLINE sepBy1 #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
sepBy1_ :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
sepBy1_ p s = M.liftM2 (:) p (many_ (s >> p))
{-# INLINE sepBy1_ #-}

-- | Applies the parser and returns its result, but resets
-- the 'Stream' as if it consumed nothing.
lookAhead :: Stream s t => Parser s a -> Parser s a
lookAhead v@(Parser p) = Parser $ \s -> 
    let 
        g (Done a _) = Done a s
        g (Partial p') = Partial $ \s' -> 
            case s' of
                Nothing -> p' Nothing
                Just s'' -> parse (lookAhead v) (streamAppend s s') 
    in
        map g (p s) 
{-# INLINE lookAhead #-}

-- | @count n p@ parses exactly @n@ occurences of @p@.
count :: Stream s t => Int -> Parser s a -> Parser s [a]
count = exactly
{-# INLINE count #-}

-- | Identical to 'many' except the result is discarded.
skipMany :: Stream s t => Parser s a -> Parser s ()
skipMany = M.void . many
{-# INLINE skipMany #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
skipMany_ :: Stream s t => Parser s a -> Parser s ()
skipMany_ = M.void . many_
{-# INLINE skipMany_ #-}

-- | Identical to 'many1' except the result is discarded.
skipMany1 :: Stream s t => Parser s a -> Parser s ()
skipMany1 = M.void . many1
{-# INLINE skipMany1 #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
skipMany1_ :: Stream s t => Parser s a -> Parser s ()
skipMany1_ = M.void . many1_
{-# INLINE skipMany1_ #-}

-- | @endBy p s@ parses multiple occurences of @p@ separated and ended by
-- @s@.
endBy :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
endBy p s = many (p A.<* s)
{-# INLINE endBy #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
endBy_ :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
endBy_ p s = many_ (p A.<* s)
{-# INLINE endBy_ #-}

-- | @endBy1 p s@ parses one or more occurences of @p@ separated and ended 
-- by @s@.
endBy1 :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
endBy1 p s = many1 (p A.<* s)
{-# INLINE endBy1 #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
endBy1_ :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
endBy1_ p s = many1_ (p A.<* s)
{-# INLINE endBy1_ #-}

-- | @sepEndBy p s@ parses multiple occurences of @p@ separated and 
-- optionally ended by @s@.
sepEndBy :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
sepEndBy p s = sepBy p s A.<* optional s
{-# INLINE sepEndBy #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
sepEndBy_ :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
sepEndBy_ p s = sepBy_ p s A.<* optional s
{-# INLINE sepEndBy_ #-}

-- | @sepEndBy p s@ parses one or more occurences of @p@ separated and 
-- optionally ended by @s@.
sepEndBy1 :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
sepEndBy1 p s = sepBy1 p s A.<* optional s
{-# INLINE sepEndBy1 #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
sepEndBy1_ :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
sepEndBy1_ p s = sepBy1_ p s A.<* optional s
{-# INLINE sepEndBy1_ #-}

-- | @chainr p o x@ parses zero or more occurences of @p@ separated by @o@. 
-- The result is the left associative application of the functions to the
-- values. If @p@ succeeds zero times, @x@ is returned.
chainl :: Stream s t => Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainl p o x = chainl1 p o <|> return x
{-# INLINE chainl #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
chainl_ :: Stream s t => Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainl_ p o x = chainl1_ p o <||> return x
{-# INLINE chainl_ #-}

-- | Like 'chainl', but a minimum of one occurence of @p@ must be parsed.
chainl1 :: Stream s t => Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl1 p o = p >>= f
    where
        f x =
            do
                g <- o
                y <- p
                f (g x y)
            <|> return x
{-# INLINE chainl1 #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
chainl1_ :: Stream s t => Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl1_ p o = p >>= f
    where
        f x =
            do
                g <- o
                y <- p
                f (g x y)
            <||> return x
{-# INLINE chainl1_ #-}

-- | Like 'chainl', but right associative.
chainr :: Stream s t => Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainr p o x = chainr1 p o <|> return x
{-# INLINE chainr #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
chainr_ :: Stream s t => Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainr_ p o x = chainr1_ p o <||> return x
{-# INLINE chainr_ #-}

-- | Like 'chainl1', but right associative.
chainr1 :: Stream s t => Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr1 p o = p >>= f
    where
        f x = 
            do
                g <- o
                y <- chainr1 p o
                return (g x y)
            <|> return x
{-# INLINE chainr1 #-}

-- | Branches every iteration where one branch stops and one branch 
-- continues.
chainr1_ :: Stream s t => Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr1_ p o = p >>= f
    where
        f x = 
            do
                g <- o
                y <- chainr1_ p o
                return (g x y)
            <||> return x
{-# INLINE chainr1_ #-}

-- | Only succeeds when the given parser fails. Consumes no input.
notFollowedBy :: Stream s t => Parser s a -> Parser s ()
notFollowedBy p = test p >>= assert . not
{-# INLINE notFollowedBy #-}

-- | @manyTill a b@ parses multiple occurences of @a@ until @b@ would 
-- succeed if tried.
manyTill :: Stream s t => Parser s a -> Parser s b -> Parser s [a]
manyTill p e = 
    do
        b <- test e
        if b 
            then return []
            else M.liftM2 (:) p (manyTill p e)
{-# INLINE manyTill #-}

-- | Does nothing -- only used for @Parsec@ compatability.
try :: Stream s t => Parser s a -> Parser s a
try = id
{-# INLINE try #-}

-- | @eitherP a b@ returns the result wrapped in a 'Left' if @a@ succeeds or
-- a 'Right' if @b@ succeeds
eitherP :: Stream s t => Parser s a -> Parser s b -> Parser s (Either a b)
eitherP a b = M.liftM Left a <|> M.liftM Right b
{-# INLINE eitherP #-}

-- | Like 'eitherP', but tries both @a@ and @b@.
eitherP_ :: Stream s t => Parser s a -> Parser s b -> Parser s (Either a b)
eitherP_ a b = M.liftM Left a <||> M.liftM Right b
{-# INLINE eitherP_ #-}

-- | Parses a sequence of parsers in any order.
perm :: Stream s t => [Parser s a] -> Parser s [a]
perm [] = return []
perm ps = 
    do
        (i, r) <- select ps
        M.liftM (r:) (perm (let (a,b) = splitAt i ps in a ++ tail b)) 
{-# INLINE perm #-}

-- | Parses a sequence of parsers in all possible orders.
perm_ :: Stream s t => [Parser s a] -> Parser s [a]
perm_ [] = return []
perm_ ps = 
    do
        (i, r) <- select_ ps
        M.liftM (r:) (perm_ (let (a,b) = splitAt i ps in a ++ tail b)) 
{-# INLINE perm_ #-}
