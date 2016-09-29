module SimpleFunctions where

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
split (a:b:abs) = (a:as, b:bs)
    where (as, bs) = split abs
-- this works but there is probs a cooler way

--split a = (map ((!!) a)[0,2..(length a - 1)],map ((!!) a)[1,3..(length a - 1)])
--cooler? ...idk


-- d)
interleave :: ([a],[a]) -> [a]
interleave ([], a)   = a
interleave (a, [])   = a
interleave (a:as, b) = a : interleave (b, as)

-- e)
merge :: (Ord a) => ([a],[a]) -> [a]
merge ([], a) = a
merge (a, []) = a
merge (a:as, b:bs)
    | a < b     = a : merge (as, b:bs)
    | otherwise = b : merge (a:as, bs)

-- f)
mergeSort :: (Ord a) => [a] -> [a]
mergeSort []  = []
mergeSort [a] = [a]
mergeSort a   =
    merge ((mergeSort as), (mergeSort bs))
    where (as, bs) = split a

