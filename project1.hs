import Data.Char



data Piece = Piece { letter :: Char, x :: Int, y :: Int} deriving Show

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

--generateMoves :: [Piece] -> [[Piece]]
--generateMoves 

-- check to see if generated pieces are valid given reference 
-- generateImmediatePieceMoves piece reference = isValid(reference generateImmediatePieceMoves' piece)

-- this does not currently include jumps, only direct moves
generateImmediatePieceMoves' :: Piece -> [Piece]
generateImmediatePieceMoves' piece = 
    case piece of (Piece letter x y) -> (Piece letter (x + 1) y):(Piece letter (x + 1 ) (y + 1)): (Piece letter (x + 1) (y - 1)):[]

