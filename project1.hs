import Data.Char
import Data.Maybe

testPiece1 = (Piece 'w' 0 0)

whiteBoard = makeInternalRep_b4b8 ["----","---","-w","---","----"]
startBoard = makeInternalRep_b4b8 ["www-","--w","--","---","bbbb"]
startBoardNoRep = ["www-","--w","--","---","bbbb"]

data Piece = Piece { letter :: Char, x :: Int, y :: Int} deriving (Show, Eq)

-- A type used to refer to a whole playing board
type Board = [Piece]

-- oska_b4b8 :: [String] -> Char -> Int -> [String]
-- oska_b4b8 board colour depth

{- BOARD REPRESENTATION FUNCTIONS -}
-- Takes a board (as a list of strings) and returns a list of Pieces
makeInternalRep_b4b8 :: [String] -> [Piece]
makeInternalRep_b4b8 board = createPieces_b4b8 board 0

-- Takes a board and its size and returns it as external representation
makeExternalRep_b4b8 :: Board -> Int -> [String]
makeExternalRep_b4b8 board size = makeExternalRep'_b4b8 board (createBlankBoard_b4b8 size)

makeExternalRep'_b4b8 :: Board -> [String] -> [String]
makeExternalRep'_b4b8 [] strBoard = strBoard
makeExternalRep'_b4b8 board strBoard =
    case (head board) of (Piece letter x y) -> makeExternalRep'_b4b8
                                               (tail board)
                                               (replaceCharAtIndex y x letter strBoard)
                                               
                                               

createBlankBoard_b4b8 :: Int -> [String]
createBlankBoard_b4b8 size = (createBlankHalf_b4b8 (size-1)) ++ ["--"] ++ (reverse (createBlankHalf_b4b8 (size-1)))

createBlankHalf_b4b8 :: Int -> [String]
createBlankHalf_b4b8 size
    | size <= 2 = []
    | otherwise =  replicate size '-' : createBlankHalf_b4b8 (size-1)

-- Given x, y coordinate of string board, replace with char in given boad
replaceCharAtIndex :: Int -> Int -> Char -> [String] -> [String]
replaceCharAtIndex x y char sboard = a ++ ((c ++ (char:rest)):b)
    where (a, (row:b)) = splitAt y sboard
          (c, (_:rest)) = splitAt x row

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
generateMoves_b4b8 letter board boardSize = concat[generateImmediateMoves_b4b8 letter board board boardSize,
                                                   generateJumpMoves_b4b8 letter board board boardSize]


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
-- Generate jump moves for a colour, list of pieces and a reference board.
generateJumpMoves_b4b8 :: Char -> [Piece] -> Board -> Int -> [Board]
generateJumpMoves_b4b8 _ [] _ _            = []
generateJumpMoves_b4b8 letter lop refBoard size
    | getLetter_b4b8 (head lop) == letter  = generateJumpMoves'_b4b8 (head lop) refBoard size ++
                                            generateJumpMoves_b4b8 letter (tail lop) refBoard size
    | otherwise                            = generateJumpMoves_b4b8 letter (tail lop) refBoard size

generateJumpMoves'_b4b8 :: Piece -> Board -> Int -> [Board]
generateJumpMoves'_b4b8 piece refBoard size
    | getX_b4b8 piece < (size `div` 2) - 1  = catMaybes (map (\new -> validateJump_b4b8 piece new refBoard size) ((Piece letter (x+2) y) : (Piece letter (x+2) (y-2)) : []))
    | getX_b4b8 piece == (size `div` 2)- 1  = catMaybes (map (\new -> validateJump_b4b8 piece new refBoard size) ((Piece letter (x+2) (y-1)) : (Piece letter (x+2) (y+1)) : []))
    | getX_b4b8 piece >= (size `div` 2)     = catMaybes (map (\new -> validateJump_b4b8 piece new refBoard size) ((Piece letter (x+2) y) : (Piece letter (x+2) (y+2) : [])))
    | otherwise                             = []
    where x = getX_b4b8 piece
          y = getY_b4b8 piece
          letter = getLetter_b4b8 piece

-- Returns a Just board if a jump is valid. Nothing if invalid
validateJump_b4b8 :: Piece -> Piece -> Board -> Int -> Maybe Board
validateJump_b4b8 old new board size
    | not (isValidLocation_b4b8 new board size)  = Nothing
    | isJust pieceBetween                        = if getLetter_b4b8 (fromJust pieceBetween) == getLetter_b4b8 old
                                                   then Nothing
                                                   else Just (removePiece_b4b8 (fromJust pieceBetween) (swap_b4b8 new old board))
    | otherwise                                  = Nothing
    where pieceBetween = getPieceBetween_b4b8 old new board

{- BOARD OPERATIONS -}
-- Swaps a given piece with another piece on the board
swap_b4b8 :: Piece -> Piece -> Board -> Board
swap_b4b8 new old board = new : (filter (/= old) board)

-- Checks to see if there is a piece at the specified coordinate on the board
isPieceAt_b4b8 :: Int -> Int -> Board -> Bool
isPieceAt_b4b8 _ _ []     = False
isPieceAt_b4b8 x y board  = 
    case (head board) of (Piece _ px py) -> if px == x && py == y
                                            then True
                                            else isPieceAt_b4b8 x y (tail board)

-- Get a piece between two pieces
-- NOTE: If jump is broken... this function is probably what's causing it
getPieceBetween_b4b8 :: Piece -> Piece -> Board -> Maybe Piece
getPieceBetween_b4b8 p1 p2 board
    | getY_b4b8 p1 == getY_b4b8 p2            = getPieceAt_b4b8 ((getX_b4b8 p2) - 1) (getY_b4b8 p2) board
    | getY_b4b8 p1 == (getY_b4b8 p2) - 2      = getPieceAt_b4b8 ((getX_b4b8 p2) - 1) ((getY_b4b8 p2) - 1) board
    | getY_b4b8 p1 == (getY_b4b8 p2) + 2      = getPieceAt_b4b8 ((getX_b4b8 p2) - 1) ((getY_b4b8 p2) + 1) board
    | (getY_b4b8 p1) == ((getY_b4b8 p2) - 1)  = getPieceAt_b4b8 ((getX_b4b8 p2) - 1) ((getY_b4b8 p2) - 1) board
    | (getY_b4b8 p1) == ((getY_b4b8 p2) + 1)  = getPieceAt_b4b8 ((getX_b4b8 p2) - 1) (getY_b4b8 p2) board
    | otherwise                               = Nothing 

-- Returns a piece at the specified coordinates
getPieceAt_b4b8 :: Int -> Int -> Board -> Maybe Piece
getPieceAt_b4b8 _ _ [] = Nothing
getPieceAt_b4b8 x y board = 
    case (head board) of piece@(Piece _ px py) -> if px == x && py == y
                                                  then Just piece 
                                                  else getPieceAt_b4b8 x y (tail board)

-- Remove a piece from the board
removePiece_b4b8 :: Piece -> Board -> Board
removePiece_b4b8 _ []        = []
removePiece_b4b8 piece board
    | (head board) == piece  = removePiece_b4b8 piece (tail board)
    | otherwise              = (head board) : removePiece_b4b8 piece (tail board)


-- Checks to see if a given piece has valid coordinates and if its coordinates are vacant
isValidLocation_b4b8 :: Piece -> Board -> Int -> Bool
isValidLocation_b4b8 piece board size
    | isPieceAt_b4b8 (getX_b4b8 piece) (getY_b4b8 piece) board                              = False
    | getY_b4b8 piece < 0 || getX_b4b8 piece >= size                                        = False
    | getX_b4b8 piece < (size `div` 2) && getY_b4b8 piece > (size - 1 - (getX_b4b8 piece))  = False
    | getX_b4b8 piece >= (size `div` 2) && getY_b4b8 piece >= getX_b4b8 piece               = False
    | otherwise                                                                             = True

-- "Rotate" the board 180 degrees
turnBoard_b4b8 :: Board -> Int -> Board
turnBoard_b4b8 [] _                    = []
turnBoard_b4b8 (piece:rest) size
    | getY_b4b8 piece > size `div` 2   = (Piece (getLetter_b4b8 piece) (maxX - (getX_b4b8 piece)) (maxY - (getY_b4b8 piece))):
                                         turnBoard_b4b8 rest size
    | getY_b4b8 piece <= size `div` 2  = (Piece (getLetter_b4b8 piece) (maxX - (getX_b4b8 piece)) (maxY - (getX_b4b8 piece) - (getY_b4b8 piece))):
                                         turnBoard_b4b8 rest size
    where maxX = size - 1
          maxY = size - 2

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

{- BOARD EVALUATOR -}
-- given a board, will determine if it's in favour of opponent or self
-- positive value means in favour of self
-- negative value means in favour of opponent
-- heuristic is calculated by
-- [(# of own pieces) - (# of opponents pieces)] + [(opponents closest x-coor) - (n - (own's closest x-coor))]
--			where n = # of rows in the board/ board size
boardEval_b4b8 :: [String] -> Char -> Int
boardEval_b4b8 [] char = 0
boardEval_b4b8 board char
	| (char /= 'w') && (char /= 'b') = 0
        | isWon_b4b8 board (length board) char = 10000
    -- There's gotta be a better way of doing this...
        | isWon_b4b8 (makeExternalRep_b4b8 (turnBoard_b4b8 (makeInternalRep_b4b8 board) size) size) size (oppColour_b4b8 char) = -100000
	| otherwise			 = (charCount_b4b8 board char) + (closestCount_b4b8 board (getBoardSize_b4b8 board) char)
        where size = length board

-- Checks if top player has won (either white or black)
isWon_b4b8 :: [String] -> Int -> Char -> Bool
isWon_b4b8 board size char
    | char == 'w' = if numBlack == 0 || length (filter (== char) (board !! (size-1))) == numWhite
                    then True
                    else False
    | char == 'b' = if numWhite == 0 || length (filter (== char) (board !! (size-1))) == numBlack
                    then True
                    else False
    | otherwise = False
    where numWhite = wCount_b4b8 (concat board) 0
          numBlack = bCount_b4b8 (concat board) 0

-- Given a colour returns the opposite colour
oppColour_b4b8 :: Char -> Char
oppColour_b4b8 colour
    | colour == 'w' = 'b'
    | otherwise     = 'w'

-- return (opponents closest x-coor) - (size - (own's closest x-coor))
closestCount_b4b8 :: [String] -> Int -> Char -> Int
closestCount_b4b8 board size char 
	| char == 'w' = (getSmallestB_b4b8 (makeInternalRep_b4b8 board) size 'b') - 
			(size - (getBiggestW_b4b8 (makeInternalRep_b4b8 board) 0 'w'))
	| otherwise   = (getSmallestW_b4b8 (makeInternalRep_b4b8 board) size 'w') - 
			(size - (getBiggestB_b4b8 (makeInternalRep_b4b8 board) 0 'b'))


--return largest W x-coordinate
getBiggestW_b4b8 :: [Piece] -> Int -> Char -> Int
getBiggestW_b4b8 [] max _ = max
getBiggestW_b4b8 pieces max 'w' =
	case (head pieces) of 
							(Piece 'w' x y) -> if (x > max)
											then getBiggestW_b4b8 (tail pieces) x 'w'
											else getBiggestW_b4b8 (tail pieces) max 'w'
							(Piece 'b' x y) -> getBiggestW_b4b8 (tail pieces) max 'w'

--return largest B x-coordinate
getBiggestB_b4b8 :: [Piece] -> Int -> Char -> Int
getBiggestB_b4b8 [] max _ = max
getBiggestB_b4b8 pieces max 'b' =
	case (head pieces) of 
							(Piece 'b' x y) -> if (x > max)
											then getBiggestB_b4b8 (tail pieces) x 'b'
											else getBiggestB_b4b8 (tail pieces) max 'b'
							(Piece 'w' x y) -> getBiggestB_b4b8 (tail pieces) max 'b'

-- return smallest W x-coordinate
getSmallestW_b4b8 :: [Piece] -> Int -> Char -> Int
getSmallestW_b4b8 [] min _ = min
getSmallestW_b4b8 pieces min 'w' =
	case (head pieces) of 
							(Piece 'w' x y) -> if (x < min)
											then getSmallestW_b4b8 (tail pieces) x 'w'
											else getSmallestW_b4b8 (tail pieces) min 'w'
							(Piece 'b' x y) -> getSmallestW_b4b8 (tail pieces) min 'w'

-- returns (# of my pieces) - (# of opponents pieces)
charCount_b4b8 :: [String] -> Char -> Int
charCount_b4b8 board char
	| char == 'w'				= (wCount_b4b8 (concat board) 0) - (bCount_b4b8 (concat board) 0)
	| otherwise					= (bCount_b4b8 (concat board) 0) - (wCount_b4b8 (concat board) 0)

-- count # of w's in board
wCount_b4b8 :: [Char] -> Int -> Int
wCount_b4b8 board count
	| null board     		=  count
	| (head board) == 'w' 	=  wCount_b4b8 (tail board) (count + 1)
	| otherwise				= wCount_b4b8 (tail board) count

-- count # of w's in board
bCount_b4b8 :: [Char] -> Int -> Int
bCount_b4b8 board count
	| null board     		=  count
	| (head board) == 'b' 	=  bCount_b4b8 (tail board) (count + 1)
	| otherwise				= bCount_b4b8 (tail board) count
		
--return smallest B x-coordinate
getSmallestB_b4b8 :: [Piece] -> Int -> Char -> Int
getSmallestB_b4b8 [] min _ = min
getSmallestB_b4b8 pieces min 'b' =
	case (head pieces) of 
							(Piece 'b' x y) -> if (x < min)
											then getSmallestB_b4b8 (tail pieces) x 'b'
											else getSmallestB_b4b8 (tail pieces) min 'b'
							(Piece 'w' x y) -> getSmallestB_b4b8 (tail pieces) min 'b'

-- return how many rows there are in the board, starting with index 0
getBoardSize_b4b8 :: [String] -> Int
getBoardSize_b4b8 [] = 0
getBoardSize_b4b8 board = getBoardSize'_b4b8 board 0

getBoardSize'_b4b8 :: [String] -> Int -> Int
getBoardSize'_b4b8 board size
	| null board 				= (size - 1)
	| otherwise					= getBoardSize'_b4b8 (tail board) (size + 1)

	

