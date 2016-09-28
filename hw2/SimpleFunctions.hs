module SimpleFunctions where

import Data.List
import System.IO

-- a)
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst b (a:as)
    | b a       = a : filterFirst b as
    | otherwise = as

-- b)
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast _ [] = []
filterLast b a
    | b (last a) = filterLast b (init a) ++ [last a]
    | otherwise  = init a

-- c)
split :: [a] -> ([a],[a])
split []        = ([], [])
split [a]       = ([a], [])
--split (a:b:abs) = (a:as, b:bs)
--    where (as, bs) = split abs
-- this works but there is probs a cooler way

split a = (map ((!!) a)[0,2..(length a - 1)],map ((!!) a)[1,3..(length a - 1)])
--cooler? ...idk


-- d)
interleave :: ([a],[a]) -> [a]
interleave ([], a)  = a
interleave (a, []) = a
interleave (x:xs, y:ys) = x : y : interleave (xs, ys)

-- (map ((!!) a)[0,2..(length a - 1)],map ((!!) a)[1,3..(length a - 1)])

---- e)
--merge :: (Ord a) => ([a],[a]) -> [a]
--
---- f)
--mergeSort :: (Ord a) => [a] -> [a]


