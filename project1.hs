import Data.Char

testPiece1 = (Piece 'w' 0 0)
whiteBoard = makeInternalRep_b4b8 ["----","---","--","w--","----"]

data Piece = Piece { letter :: Char, x :: Int, y :: Int} deriving (Show, Eq)

-- Takes a board (as a list of strings) and returns a list of Pieces
makeInternalRep_b4b8 :: [String] -> [Piece]
makeInternalRep_b4b8 board = makeCoordinates_b4b8 board 0

makeCoordinates_b4b8 :: [String] -> Int -> [Piece]
makeCoordinates_b4b8 board rowIndex
    | null board = []
    | otherwise = makeCoordinates_b4b8' (head board) rowIndex 0 ++
                    makeCoordinates_b4b8 (tail board) (rowIndex + 1)

makeCoordinates_b4b8' :: String -> Int -> Int -> [Piece]
makeCoordinates_b4b8' row rowIndex colIndex
    | null row = []
    | isAlpha (head row) = (Piece (head row) rowIndex colIndex) :
                            makeCoordinates_b4b8' (tail row) rowIndex (colIndex + 1)
    | otherwise = makeCoordinates_b4b8' (tail row) rowIndex (colIndex + 1)

-- Generates all possible states for a given colour, board and the size of the board. 
-- TODO: Add more move generators in concat
generateMoves_b4b8 :: Char -> [Piece] -> Int -> [[Piece]]
generateMoves_b4b8 letter board size =
    concat [generateImmediatePieceMoves_b4b8 letter board board size]

-- Generate states for each piece on board. Return the list of immediate states
generateImmediatePieceMoves_b4b8 :: Char -> [Piece] -> [Piece] -> Int -> [[Piece]]
generateImmediatePieceMoves_b4b8 _ _ [] _ = []
generateImmediatePieceMoves_b4b8 letter ref board size
    | getLetter_b4b8 (head board) == letter = generateImmediatePieceMoves'_b4b8 (head board) ref size ++
                                              generateImmediatePieceMoves_b4b8 letter ref (tail board) size
    | otherwise                             = generateImmediatePieceMoves_b4b8 letter ref (tail board) size

-- Given a piece and a reference board, and size of the board generate a list of possible move states for that piece
generateImmediatePieceMoves'_b4b8 :: Piece -> [Piece] -> Int -> [[Piece]]
generateImmediatePieceMoves'_b4b8 piece ref size
    | getX_b4b8 piece < size `div` 2 = swapImmediateMoves_b4b8 
                                       piece 
                                       (genAllImmediate_b4b8
                                        piece
                                        (generateImmediatePieceMoves_b4b8' piece)
                                        isValidImmediateTopBoard_b4b8)
                                       ref
    | getX_b4b8 piece >=  size `div` 2 = swapImmediateMoves_b4b8 piece (genAllImmediate_b4b8
                                     piece
                                     (generateImmediatePieceMoves_b4b8' piece)
                                     isValidImmediateBottomBoard_b4b8) ref
    | otherwise = []

-- Given a list of pieces and a board, swaps old new with board                                  
swapImmediateMoves_b4b8 :: Piece -> [Piece] -> [Piece] -> [[Piece]]
swapImmediateMoves_b4b8 _ [] _ = []
swapImmediateMoves_b4b8 piece (m:ms) ref = swap_b4b8 m piece ref : swapImmediateMoves_b4b8 piece ms ref

-- Given a piece and a list of possible moves for that piece, creates a list of valid moves
genAllImmediate_b4b8 :: Piece -> [Piece] -> (Piece -> Piece -> Bool) -> [Piece] 
genAllImmediate_b4b8 piece pieces validationFunction
    | null pieces = []
    | validationFunction piece (head pieces) = (head pieces) : genAllImmediate_b4b8 piece (tail pieces) validationFunction
    | otherwise = genAllImmediate_b4b8 piece (tail pieces) validationFunction

-- this does not currently include jumps, only direct moves
generateImmediatePieceMoves_b4b8' :: Piece -> [Piece]
generateImmediatePieceMoves_b4b8' piece = 
    case piece of (Piece letter x y) -> (Piece letter (x + 1) y):(Piece letter (x + 1 ) (y + 1)): (Piece letter (x + 1) (y - 1)):[]

-- Swaps out a old piece for a new piece in a board
swap_b4b8 :: Piece -> Piece -> [Piece] -> [Piece]
swap_b4b8 new old board = new : (filter (/= old) board)

-- Immediate move validation on top half of board
isValidImmediateTopBoard_b4b8 :: Piece -> Piece -> Bool
isValidImmediateTopBoard_b4b8 reference new
	| (((getY_b4b8 new) == ((getY_b4b8 reference) - 1)) && (getY_b4b8 reference == 0)) = False
	| (getY_b4b8 new) == ((getY_b4b8 reference) + 1) = False
	| otherwise = True

-- Immediate move validation on bottom half of board
isValidImmediateBottomBoard_b4b8 :: Piece -> Piece -> Bool
isValidImmediateBottomBoard_b4b8 reference new
	| ((getY_b4b8 new) == ((getY_b4b8 reference) - 1)) 		= False
	| otherwise = True

--- GETTER FUNCTIONS
getX_b4b8 :: Piece -> Int
getX_b4b8 piece =
	case piece of (Piece letter x y) -> x

getY_b4b8 :: Piece -> Int
getY_b4b8 piece =
	case piece of (Piece letter x y) -> y

getLetter_b4b8 :: Piece -> Char
getLetter_b4b8 piece =
	case piece of (Piece letter x y) -> letter

--- SETTER FUNCTIONS

setX_b4b8 :: Int -> Piece -> Piece
setX_b4b8 value oldPiece = 
	case oldPiece of (Piece letter x y) -> (Piece letter value y)

setY_b4b8 :: Int -> Piece -> Piece
setY_b4b8 value oldPiece = 
	case oldPiece of (Piece letter x y) -> (Piece letter x value)

setLetter_b4b8 :: Char -> Piece -> Piece
setLetter_b4b8 value oldPiece = 
	case oldPiece of (Piece letter  x y) -> (Piece value x y)



