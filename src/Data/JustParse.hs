module Data.JustParse (
    module X,
    regex
) where

import Data.JustParse.Regex 
import Data.JustParse.Common as X
import Data.Monoid
import Data.Maybe
import Control.Monad

regex :: Stream s Char => String -> Parser s Char Match
regex s 
    | null r = mzero
    | isJust $ leftover $ head r = mzero
    | otherwise = value $ head r
    where
        r = finalize (parse (greedy regular) (Just s))



