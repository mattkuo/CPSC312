import Data.Char

testPiece1 = (Piece 'w' 0 0)

data Piece = Piece { letter :: Char, x :: Int, y :: Int} deriving (Show, Eq)

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

generateMoves_b4b8 :: [Piece] -> [[Piece]]
generateMoves_b4b8 pieces
    | null pieces = []
    | otherwise = generateImmediatePieceMoves_b4b8 strBoard (head pieces) : generateMoves_b4b8 (tail pieces)

-- check to see if generated pieces are valid given reference 
generateImmediatePieceMoves_b4b8 :: [String] -> Piece -> [Piece]
generateImmediatePieceMoves_b4b8 reference piece =
    case piece of (Piece letter x y) -> | x < ((length reference) / 2) = if isValidImmediateTopBoard_b4b8 
                                                                         then 
                                        | x > ((length reference) / 2) = if isValidImmediateBottomBoard_b4b8
    if length reference) / 2  then expression else expression
    | ( == 
    (length reference)
(generateImmediatePieceMoves_b4b8' piece)

function_b4b8 :: Piece -> [Piece] -> [Piece]
function_b4b8 piece lop
    | null lop = []
    | generateImmediatePieceMoves_b4b8' piece


function2_b4b8 :: Piece -> 

-- this does not currently include jumps, only direct moves
generateImmediatePieceMoves_b4b8' :: Piece -> [Piece]
generateImmediatePieceMoves_b4b8' piece = 
    case piece of (Piece letter x y) -> (Piece letter (x + 1) y):(Piece letter (x + 1 ) (y + 1)): (Piece letter (x + 1) (y - 1)):[]

swap_b4b8 :: Piece -> Piece -> [Piece] -> [Piece]
swap_b4b8 new old board = new : (filter (/= old) board)

-- Immediate move validation on top half of board
isValidImmediateTopBoard_b4b8 :: Piece -> Piece -> Bool
isValidImmediateTopBoard_b4b8 reference new
	| (((getY_b4b8 new) == ((getY_b4b8 reference) - 1)) && (getY_b4b8 reference == 0)) 		= False
	| (getY_b4b8 new) == ((getY_b4b8 reference) + 1) 										= False
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



