module Chess where

-- See https://en.wikipedia.org/wiki/Chess for more details
-- Recall we only consider the situation where there is only a single
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
-- implement isValidPosition
-- valid row    indices are: a,b,c,...,h
-- valid column indices are: 1,2,3,...,8
-- change the code below
isLegalPosition (col, row) = 
    if (col == 'a' || col == 'b' || col == 'c' || col == 'd' || col == 'e' || col == 'f' || col == 'g' || col == 'h')
        then if row >= 1 && row <= 8 
            then True else False
        else False
        

-- see Rules - Movement for legal movements 

isLegalMove :: Color -> Piece -> Position -> Position -> Bool
-- implement isLegalMove

-- calculate abs value between two column chars
helperDeltaColumn :: Char -> Char -> Int
helperDeltaColumn 'a' 'a' = 0
helperDeltaColumn 'a' 'b' = 1
helperDeltaColumn 'a' 'c' = 2
helperDeltaColumn 'a' 'd' = 3
helperDeltaColumn 'a' 'e' = 4
helperDeltaColumn 'a' 'f' = 5
helperDeltaColumn 'a' 'g' = 6
helperDeltaColumn 'a' 'h' = 7

helperDeltaColumn 'b' 'a' = 1
helperDeltaColumn 'b' 'b' = 0
helperDeltaColumn 'b' 'c' = 1
helperDeltaColumn 'b' 'd' = 2
helperDeltaColumn 'b' 'e' = 3
helperDeltaColumn 'b' 'f' = 4
helperDeltaColumn 'b' 'g' = 5
helperDeltaColumn 'b' 'h' = 6

helperDeltaColumn 'c' 'a' = 2
helperDeltaColumn 'c' 'b' = 1
helperDeltaColumn 'c' 'c' = 0
helperDeltaColumn 'c' 'd' = 1
helperDeltaColumn 'c' 'e' = 2
helperDeltaColumn 'c' 'f' = 3
helperDeltaColumn 'c' 'g' = 4
helperDeltaColumn 'c' 'h' = 5

helperDeltaColumn 'd' 'a' = 3
helperDeltaColumn 'd' 'b' = 2
helperDeltaColumn 'd' 'c' = 1
helperDeltaColumn 'd' 'd' = 0
helperDeltaColumn 'd' 'e' = 1
helperDeltaColumn 'd' 'f' = 2
helperDeltaColumn 'd' 'g' = 3
helperDeltaColumn 'd' 'h' = 4

helperDeltaColumn 'e' 'a' = 4
helperDeltaColumn 'e' 'b' = 3
helperDeltaColumn 'e' 'c' = 2
helperDeltaColumn 'e' 'd' = 1
helperDeltaColumn 'e' 'e' = 0
helperDeltaColumn 'e' 'f' = 1
helperDeltaColumn 'e' 'g' = 2
helperDeltaColumn 'e' 'h' = 3

helperDeltaColumn 'f' 'a' = 5
helperDeltaColumn 'f' 'b' = 4
helperDeltaColumn 'f' 'c' = 3
helperDeltaColumn 'f' 'd' = 2
helperDeltaColumn 'f' 'e' = 1
helperDeltaColumn 'f' 'f' = 0
helperDeltaColumn 'f' 'g' = 1
helperDeltaColumn 'f' 'h' = 2

helperDeltaColumn 'g' 'a' = 6
helperDeltaColumn 'g' 'b' = 5
helperDeltaColumn 'g' 'c' = 4
helperDeltaColumn 'g' 'd' = 3
helperDeltaColumn 'g' 'e' = 2
helperDeltaColumn 'g' 'f' = 1
helperDeltaColumn 'g' 'g' = 0
helperDeltaColumn 'g' 'h' = 1

helperDeltaColumn 'h' 'a' = 7
helperDeltaColumn 'h' 'b' = 6
helperDeltaColumn 'h' 'c' = 5
helperDeltaColumn 'h' 'd' = 4
helperDeltaColumn 'h' 'e' = 3
helperDeltaColumn 'h' 'f' = 2
helperDeltaColumn 'h' 'g' = 1
helperDeltaColumn 'h' 'h' = 0
    
-- calculate legal moves
isLegalMove White King (cr,rr) (ct,rt) = 
    if (helperDeltaColumn cr ct) <= 1
        then if (abs (rr - rt)) <= 1
            then True else False
        else False
isLegalMove Black King (cr,rr) (ct,rt) = isLegalMove White King (cr,rr) (ct,rt)

isLegalMove White Rook (cr,rr) (ct,rt) =
    if (helperDeltaColumn cr ct) == 0
        then if (abs (rr-rt)) > 0
            then True
            else False
        else if (abs (rr-rt)) == 0
            then True
            else False
isLegalMove Black Rook (cr,rr) (ct,rt) = isLegalMove White Rook (cr,rr) (ct,rt)

isLegalMove White Pawn (cr,rr) (ct,rt) = 
    if cr == ct
        then if ((rt - rr) <= 2 && (rt - rr) > 0)
            then True
            else False
        else False

isLegalMove Black Pawn (cr,rr) (ct,rt) =
    if cr == ct
        then if ((rr - rt) <= 2 && (rr - rt) > 0)
            then True
            else False
        else False

isLegalMove White Queen (cr,rr) (ct,rt) = 
    if rr == rt
        then if cr /= ct
            then True
            else False
        else if cr == ct
            then True
            else if (helperDeltaColumn cr ct) == (abs (subtract rr rt))
                then True
                else False
isLegalMove Black Queen (cr,rr) (ct,rt) = isLegalMove White Queen (cr,rr) (ct,rt)

isLegalMove White Bishop (cr,rr) (ct,rt) = 
    if rr == rt 
        then False
        else if (helperDeltaColumn cr ct) == (abs (subtract rr rt))
            then True
            else False
isLegalMove Black Bishop (cr,rr) (ct,rt) = isLegalMove White Bishop (cr,rr) (ct,rt)
    
isLegalMove White Knight (cr,rr) (ct,rt) = 
    if rr == rt 
        then False
        else if (abs (rr-rt)) == 1
            then if (helperDeltaColumn cr ct) == 2
                then True
                else False
            else if (abs (rr-rt)) == 2
                then if (helperDeltaColumn cr ct) == 1
                    then True
                    else False
                else False
isLegalMove Black Knight (cr,rr) (ct,rt) = isLegalMove White Knight (cr,rr) (ct,rt)
