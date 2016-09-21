module Chess where

import Data.Char


-- See https://en.wikipedia.org/wiki/Chess for more details
-- We only consider the situation where there is only a single
-- piece on the board

-- see Rules - Set up for basic definitions

type File     = Char         -- column index
                             -- valid files are 'a','b',...,'h'
type Rank     = Int          -- row index
                             -- valid ranks are 1,2,...8
type Position = (File,Rank)

data Color =
  Black | White
  deriving (Eq,Show)

data Piece =
  King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Eq,Show)

isLegalPosition :: Position -> Bool
-- implement isLegalPosition
isLegalPosition (a,b)
     | isLegalRank b && isLegalFile a = True
     | otherwise = False

isLegalFile :: File -> Bool
isLegalFile c
     | c >= 'a' && c <= 'h' = True
     | otherwise = False

isLegalRank :: Rank -> Bool
isLegalRank i
     | i >= 1 && i <= 8 = True
     | otherwise = False

-- see Rules - Movement for legal movements

isLegalMove :: Color -> Piece -> Position -> Position -> Bool
-- implement isLegalMove
isLegalMove _ King (a,b) (c,d) =
  if abs((ord a) - (ord c)) <= 1 && abs(b-d) <= 1
    && isLegalPosition  (c,d)
    then True
    else False

isLegalMove _ Queen (a,b) (c,d) =
  if abs((ord a) - (ord c)) ==0 && abs(b - d)>=1 && isLegalPosition(c,d)
    then True
    else if abs((ord a)-(ord c)) >=1 && abs(b-d) ==0 && isLegalPosition(c,d)
      then True
      else if abs((ord a) - (ord c)) == abs(b-d) && abs(b-d)/=0 && isLegalPosition(c,d)
        then True
        else False

isLegalMove _ Bishop (a,b) (c,d) =
  if abs((ord a)) - (ord c) == abs(b-d) && abs(b-d)/=0 && isLegalPosition(c,d)
    then True
    else False

isLegalMove _ Rook (a,b) (c,d) =
  if abs((ord a)-(ord c)) /=0 && abs(b-d) ==0 && isLegalPosition (c,d)
    then True
    else if abs((ord a)-(ord c)) ==0 && abs(b-d)/=0 && isLegalPosition(c,d)
      then True
      else False

isLegalMove Black Pawn (a,b) (c,d) =
  if abs((ord a)-(ord c))==0 && b - d == 1 && isLegalPosition(c,d)
    then True
    else if abs((ord a)-(ord c))==0 && b - d == 2 && b == 7 && isLegalPosition(c,d)
      then True
      else False

isLegalMove White Pawn (a,b) (c,d) =
  if abs((ord a)-(ord c))==0 && d - b == 1 && isLegalPosition(c,d)
    then True
    else if abs((ord a)-(ord c))==0 && d - b == 2 && b==2 && isLegalPosition(c,d)
      then True
      else False

isLegalMove _ Knight (a,b) (c,d) =
  if abs((ord a)-(ord c))==1 && abs(b-d)==2 && isLegalPosition(c,d)
    then True
    else if abs((ord a)-(ord c))==2 && abs(b-d) == 1 && isLegalPosition(c,d)
      then True
      else False

isLegalMove _ _ _ _ = False

doesStuff :: Char->Int
doesStuff a = ord a
