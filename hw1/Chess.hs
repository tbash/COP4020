module Chess where

-- See https://en.wikipedia.org/wiki/Chess for more details
-- We only consider the situation where there is only a single
-- piece on the board

-- see Rules - Set up for basic definitions

import Data.Char

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
isLegalPosition (f, r) = f `elem` ['a'..'h'] && r `elem` [1..8]

-- see Rules - Movement for legal movements

isLegalMove :: Color -> Piece -> Position -> Position -> Bool
isLegalMove c p (f1,r1) (f2,r2) = legalPs && case p of
    King   -> diff `elem` [(0,0),(1,0),(0,1),(1,1)]
    Queen  -> x == y || (x == 0 || y == 0)
    Rook   -> (x == 0 || y == 0)
    Bishop -> x == y
    Knight -> diff `elem` [(0,0),(2,1),(1,2)]
    Pawn   -> case c of
                Black -> pDiff `elem` [(0,0),(0,2),(0,1)]
                White -> pDiff `elem` [(0,0),(0,-2),(0,-1)]
  where legalPs = isLegalPosition (f1,r1) && isLegalPosition (f2,r2)
        diff    = (abs (ord f1 - ord f2), abs (r1 - r2))
        pDiff   = (ord f1 - ord f2, r1 - r2)
        (x,y)   = diff
