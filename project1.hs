import Data.Char
import Data.Maybe

testPiece1 = (Piece 'w' 0 0)
whiteBoard = makeInternalRep_b4b8 ["-w--","---","--","---","----"]

data Piece = Piece { letter :: Char, x :: Int, y :: Int} deriving (Show, Eq)

-- A type used to refer to a whole playing board
type Board = [Piece]

-- Takes a board (as a list of strings) and returns a list of Pieces
makeInternalRep_b4b8 :: [String] -> [Piece]
makeInternalRep_b4b8 board = createPieces_b4b8 board 0

-- Takes Board, row index and converts it into internal representation of board
createPieces_b4b8 :: [String] -> Int -> [Piece]
createPieces_b4b8 board rowIndex
    | null board = []
    | otherwise  = createPieces'_b4b8 (head board) rowIndex 0 ++
                    createPieces_b4b8 (tail board) (rowIndex + 1)

createPieces'_b4b8 :: String -> Int -> Int -> [Piece]
createPieces'_b4b8 row rowIndex colIndex
    | null row           = []
    | isAlpha (head row) = (Piece (head row) rowIndex colIndex) :
                            createPieces'_b4b8 (tail row) rowIndex (colIndex + 1)
    | otherwise          = createPieces'_b4b8 (tail row) rowIndex (colIndex + 1)


-- Generate all possible states for a given colour and board.
generateMoves_b4b8 :: Char -> Board -> Int -> [Board]
generateMoves_b4b8 letter board boardSize = generateImmediateMoves_b4b8 letter board board boardSize


{- IMMEDIATE MOVE GENERATOR -}
-- Generate immediate moves for a colour, list of pieces and a reference board.
generateImmediateMoves_b4b8 :: Char -> [Piece] -> Board -> Int -> [Board]
generateImmediateMoves_b4b8 _ [] _ _ = []
generateImmediateMoves_b4b8 colour lop refBoard size
    | getLetter_b4b8 (head lop) == colour = generateImmediateMoves'_b4b8 (head lop) refBoard size ++
                                              generateImmediateMoves_b4b8 colour (tail lop) refBoard size
    | otherwise                           = generateImmediateMoves_b4b8 colour (tail lop) refBoard size

-- Generate immediate moves for a piece given a board
generateImmediateMoves'_b4b8 :: Piece -> Board -> Int -> [Board]
generateImmediateMoves'_b4b8 piece refBoard size = 
    case piece of (Piece letter x y) -> catMaybes (map (\new -> validateImmediateMove_b4b8 piece new refBoard size) 
                                       ((Piece letter (x + 1) y):
                                       (Piece letter (x + 1) (y + 1)):
                                       (Piece letter (x + 1) (y - 1)):[]))

-- Returns a Just board if move is valid and Nothing if invalid
validateImmediateMove_b4b8 :: Piece -> Piece -> Board -> Int -> Maybe Board
validateImmediateMove_b4b8 old new board size
    | getX_b4b8 old < size `div` 2  = if (getY_b4b8 new) < 0 ||
                                         (getY_b4b8 new) > (getY_b4b8 old) ||
                                         (getY_b4b8 new) >= (size - 1 - (getX_b4b8 new)) ||
                                         isPieceAt_b4b8 (getX_b4b8 new) (getY_b4b8 new) board
                                      then Nothing
                                      else Just (swap_b4b8 new old board)
    | getX_b4b8 old >= size `div` 2 = if getY_b4b8 new == (getY_b4b8 old) - 1 ||
                                         getX_b4b8 new >= size ||
                                         isPieceAt_b4b8 (getX_b4b8 new) (getY_b4b8 new) board
                                      then Nothing
                                      else Just (swap_b4b8 new old board)
    | otherwise                     = Nothing

{- JUMP MOVE GENERATOR -}
-- Generate immediate moves for a colour, list of pieces and a reference board.
-- generateJumpMoves_b4b8 :: Char -> [Piece] -> Board -> Int -> [Board]
-- generateJumpMoves_b4b8 :: _ [] _ _ = []
-- generateJumpMoves_b4b8 letter lop refBoard size
--     | getLetter_b4b8 (head lop) == colour = generateJumpMoves'_b4b8
--     | otherwise = generateJumpMoves_b4b8 letter (tail lop) refBoard size

-- generateJumpMoves'_b4b8 :: Piece -> Board -> Int -> [Board]
-- generateJumpMoves'_b4b8 piece refBoard size =
--     case piece of (Piece letter x y) 
{- BOARD OPERATIONS -}
-- Swaps a given piece with another piece on the board
swap_b4b8 :: Piece -> Piece -> Board -> Board
swap_b4b8 new old board = new : (filter (/= old) board)

-- Checks to see if there is a piece at the specified coordinate on the board
isPieceAt_b4b8 :: Int -> Int -> Board -> Bool
isPieceAt_b4b8 _ _ [] = False
isPieceAt_b4b8 x y board = 
    case (head board) of (Piece _ px py) -> if px == x && py == y
                                            then True
                                            else isPieceAt_b4b8 x y (tail board)

{- GETTER METHODS -}
getX_b4b8 :: Piece -> Int
getX_b4b8 piece =
	case piece of (Piece letter x y) -> x

getY_b4b8 :: Piece -> Int
getY_b4b8 piece =
	case piece of (Piece letter x y) -> y

getLetter_b4b8 :: Piece -> Char
getLetter_b4b8 piece =
	case piece of (Piece letter x y) -> letter
