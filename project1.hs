import Data.Char

testPiece1 = (Piece 'w' 0 0)

data Piece = Piece { letter :: Char, x :: Int, y :: Int} deriving (Show, Eq)

makeInternalRep :: [String] -> [Piece]
makeInternalRep board = makeCoordinates board 0

makeCoordinates :: [String] -> Int -> [Piece]
makeCoordinates board rowIndex
    | null board = []
    | otherwise = makeCoordinates' (head board) rowIndex 0 ++
                    makeCoordinates (tail board) (rowIndex + 1)


makeCoordinates' :: String -> Int -> Int -> [Piece]
makeCoordinates' row rowIndex colIndex
    | null row = []
    | isAlpha (head row) = (Piece (head row) rowIndex colIndex) :
                            makeCoordinates' (tail row) rowIndex (colIndex + 1)
    | otherwise = makeCoordinates' (tail row) rowIndex (colIndex + 1)

generateMoves :: [Piece] -> [[Piece]]
generateMoves pieces
    | null pieces = []
    | otherwise = generateImmediatePieceMoves strBoard (head pieces) : generateMoves (tail pieces)

-- check to see if generated pieces are valid given reference 
generateImmediatePieceMoves :: [String] -> Piece -> [Piece]
generateImmediatePieceMoves reference piece =
    case piece of (Piece letter x y) -> | x < ((length reference) / 2) = if isValidImmediateTopBoard 
                                                                         then 
                                        | x > ((length reference) / 2) = if isValidImmediateBottomBoard
    if length reference) / 2  then expression else expression
    | ( == 
    (length reference)
(generateImmediatePieceMoves' piece)

function :: Piece -> [Piece] -> [Piece]
function piece lop
    | null lop = []
    | generateImmediatePieceMoves' piece


function2 :: Piece -> 

-- this does not currently include jumps, only direct moves
generateImmediatePieceMoves' :: Piece -> [Piece]
generateImmediatePieceMoves' piece = 
    case piece of (Piece letter x y) -> (Piece letter (x + 1) y):(Piece letter (x + 1 ) (y + 1)): (Piece letter (x + 1) (y - 1)):[]

swap :: Piece -> Piece -> [Piece] -> [Piece]
swap new old board = new : (filter (/= old) board)

-- Immediate move validation on top half of board
isValidImmediateTopBoard :: Piece -> Piece -> Bool
isValidImmediateTopBoard reference new
	| (((getY new) == ((getY reference) - 1)) && (getY reference == 0)) 		= False
	| (getY new) == ((getY reference) + 1) 										= False
	| otherwise = True

-- Immediate move validation on bottom half of board
isValidImmediateBottomBoard :: Piece -> Piece -> Bool
isValidImmediateBottomBoard reference new
	| ((getY new) == ((getY reference) - 1)) 		= False
	| otherwise = True

--- GETTER FUNCTIONS
getX :: Piece -> Int
getX piece =
	case piece of (Piece letter x y) -> x

getY :: Piece -> Int
getY piece =
	case piece of (Piece letter x y) -> y

getLetter :: Piece -> Char
getLetter piece =
	case piece of (Piece letter x y) -> letter

--- SETTER FUNCTIONS

setX :: Int -> Piece -> Piece
setX value oldPiece = 
	case oldPiece of (Piece letter x y) -> (Piece letter value y)

setY :: Int -> Piece -> Piece
setY value oldPiece = 
	case oldPiece of (Piece letter x y) -> (Piece letter x value)

setLetter :: Char -> Piece -> Piece
setLetter value oldPiece = 
	case oldPiece of (Piece letter  x y) -> (Piece value x y)



