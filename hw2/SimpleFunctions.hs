----
-- Authored By: Timothy Ashley, Damian Suski, Marcy Yi, Dax Tubach
----

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
split a = case a of
    []     -> ([], [])
    a:rest ->
      let
        (bs, as) = split rest
      in
        (a:as, bs)

-- d)
interleave :: ([a],[a]) -> [a]
interleave ([], a)   = a
interleave (a, [])   = a
interleave (a:as, b) = a : interleave (b, as)

-- e)
merge :: (Ord a) => ([a],[a]) -> [a]
merge ([], a)   = a
merge (a, [])   = a
merge (a:as, b:bs)
    | a < b     = a : merge (as, b:bs)
    | otherwise = b : merge (a:as, bs)

-- f)
mergeSort :: (Ord a) => [a] -> [a]
mergeSort a = case a of
    []  -> a
    [_] -> a
    _   ->
        let
          (as, bs) = split a
        in
          merge ((mergeSort as), (mergeSort bs))
