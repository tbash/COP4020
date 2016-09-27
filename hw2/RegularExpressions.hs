module RegularExpressions where

import Prelude hiding ((<*>))

type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (== "")

char :: Char ->  RegExp
char ch = (== [ch])

(|||) :: RegExp -> RegExp -> RegExp
(|||) e1 e2 =
  \s -> e1 s || e2 s

splits :: [a] -> [([a],[a])]
-- splits "fun" ~~>
-- [("","fun"),("f","un"),
--  ("fu","n"),("fun","")]
splits xs =
  map (flip splitAt xs) [0..length xs]
-- alternatively, we could also use a list comprehension like so
-- [splitAt i xs | i <- [0..length xs]]

(<*>) :: RegExp -> RegExp -> RegExp
(<*>) e1 e2 =
  \s -> or [ e1 prefix && e2 suffix | (prefix,suffix) <- splits s]

(<**>) :: RegExp -> RegExp -> RegExp
(<**>) e1 e2 =
  \s -> or [ e1 prefix && e2 suffix | (prefix,suffix) <- drop 1 (splits s)]

star :: RegExp -> RegExp
star e = epsilon ||| (e <**> star e)

-- put your solutions here

-- option

option :: RegExp -> RegExp

-- plus

plus :: RegExp -> RegExp

-- number

number :: RegExp

-- fractional number

fractional :: RegExp

