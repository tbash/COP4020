module SimpleFunctions where

import Data.List
import System.IO

-- a)
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ []   = []
filterFirst b (a:as)
    | b a       = a : filterFirst b as
    | otherwise = as

-- b)
--filterLast :: (a -> Bool) -> [a] -> [a]
--
---- c)
--split :: [a] -> ([a],[a])
--
---- d)
--interleave :: ([a],[a]) -> [a]
--
---- e)
--merge :: (Ord a) => ([a],[a]) -> [a]
--
---- f)
--mergeSort :: (Ord a) => [a] -> [a]


