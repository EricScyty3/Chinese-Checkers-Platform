module Board where
import Data.Maybe
import Data.List

-- the board type should be able to represent the unique identification, occupy state and the occupied piece's color
data BoardType = R Int | B Int | G Int | E Int | U deriving (Eq, Show)
type RowBoardState = [BoardType]
type Pos = (Int, Int)
-- boardWidth :: Int
-- boardWidth = 19
-- boardHeight :: Int
-- boardHeight = 13

-- determine the color state of a piece
isRed :: BoardType -> Bool
isRed (R _) = True
isRed _ = False

isBlue :: BoardType -> Bool
isBlue (B _) = True
isBlue _ = False

isGreen :: BoardType -> Bool
isGreen (G _) = True
isGreen _ = False

-- identify if a position on the board is occupied
isOccupied :: BoardType -> Maybe Bool
isOccupied U = Nothing
isOccupied (E _) = Just False
isOccupied _ = Just True

-- get the assoicated id for the piece
getIndex :: BoardType -> Int
getIndex U = -1
getIndex (R x) = x
getIndex (B x) = x
getIndex (G x) = x
getIndex (E x) = x

{-
    -- a two-dimension array representing the game board
    myList :: [RowBoardState]
    myList = [
    [U, U, U, U, U, U, U, U, U, U, R, U, U, U, U, U, U, U, U, U, U],
    [U, U, U, U, U, U, U, U, U, R, U, R, U, U, U, U, U, U, U, U, U],
    [U, U, U, U, U, U, U, U, R, U, R, U, R, U, U, U, U, U, U, U, U],
    [U, G, U, G, U, G, U, E, U, E, U, E, U, E, U, B, U, B, U, B, U],
    [U, U, G, U, G, U, E, U, E, U, E, U, E, U, E, U, B, U, B, U, U],
    [U, U, U, G, U, E, U, E, U, E, U, E, U, E, U, E, U, B, U, U, U],
    [U, U, U, U, E, U, E, U, E, U, E, U, E, U, E, U, E, U, U, U, U],
    [U, U, U, B, U, E, U, E, U, E, U, E, U, E, U, E, U, G, U, U, U],
    [U, U, B, U, B, U, E, U, E, U, E, U, E, U, E, U, G, U, G, U, U],
    [U, B, U, B, U, B, U, E, U, E, U, E, U, E, U, G, U, G, U, G, U],
    [U, U, U, U, U, U, U, U, R, U, R, U, R, U, U, U, U, U, U, U, U],
    [U, U, U, U, U, U, U, U, U, R, U, R, U, U, U, U, U, U, U, U, U],
    [U, U, U, U, U, U, U, U, U, U, R, U, U, U, U, U, U, U, U, U, U]]

    externalBoard :: [[Int]]
    externalBoard = [
    [00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 01, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00],
    [00, 00, 00, 00, 00, 00, 00, 00, 00, 02, 00, 03, 00, 00, 00, 00, 00, 00, 00, 00, 00],
    [00, 00, 00, 00, 00, 00, 00, 00, 04, 00, 05, 00, 06, 00, 00, 00, 00, 00, 00, 00, 00],
    [00, 07, 00, 08, 00, 09, 00, 10, 00, 11, 00, 12, 00, 13, 00, 14, 00, 15, 00, 16, 00],
    [00, 00, 17, 00, 18, 00, 19, 00, 20, 00, 21, 00, 22, 00, 23, 00, 24, 00, 25, 00, 00],
    [00, 00, 00, 26, 00, 27, 00, 28, 00, 29, 00, 30, 00, 31, 00, 32, 00, 33, 00, 00, 00],
    [00, 00, 00, 00, 34, 00, 35, 00, 36, 00, 37, 00, 38, 00, 39, 00, 40, 00, 00, 00, 00],
    [00, 00, 00, 41, 00, 42, 00, 43, 00, 44, 00, 45, 00, 46, 00, 47, 00, 48, 00, 00, 00],
    [00, 00, 49, 00, 50, 00, 51, 00, 52, 00, 53, 00, 54, 00, 55, 00, 56, 00, 57, 00, 00],
    [00, 58, 00, 59, 00, 60, 00, 61, 00, 62, 00, 63, 00, 64, 00, 65, 00, 66, 00, 67, 00],
    [00, 00, 00, 00, 00, 00, 00, 00, 68, 00, 69, 00, 70, 00, 00, 00, 00, 00, 00, 00, 00],
    [00, 00, 00, 00, 00, 00, 00, 00, 00, 71, 00, 72, 00, 00, 00, 00, 00, 00, 00, 00, 00],
    [00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 73, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00]]
-}

-- the overall board state for displaying and for movement changed determining
externalBoard :: [RowBoardState]
externalBoard = [
                  [U, U, U, U, U, U, U, U, U, R 1, U, U, U, U, U, U, U, U, U],
                  [U, U, U, U, U, U, U, U, R 2, U, R 3, U, U, U, U, U, U, U, U],
                  [U, U, U, U, U, U, U, R 4, U, R 5, U, R 6 , U, U, U, U, U, U, U],
                  [G 7, U, G 8, U, G 9, U, E 10, U, E 11, U, E 12, U, E 13, U, B 14, U, B 15, U, B 16],
                  [U, G 17, U, G 18, U, E 19, U, E 20, U, E 21, U, E 22, U, E 23, U, B 24, U, B 25, U],
                  [U, U, G 26, U, E 27, U, E 28, U, E 29, U, E 30, U, E 31, U, E 32, U, B 33, U, U],
                  [U, U, U, E 34, U, E 35, U, E 36, U, E 37, U, E 38, U, E 39, U, E 40, U, U, U],
                  [U, U, B 41, U, E 42, U, E 43, U, E 44, U, E 45, U, E 46, U, E 47, U, G 48, U, U],
                  [U, B 49, U, B 50, U, E 51, U, E 52, U, E 53, U, E 54, U, E 55, U, G 56, U, G 57, U],
                  [B 58, U, B 59, U, B 60, U, E 61, U, E 62, U, E 63, U, E 64, U, G 65, U, G 66, U, G 67],
                  [U, U, U, U, U, U, U, R 68, U, R 69, U, R 70, U, U, U, U, U, U, U],
                  [U, U, U, U, U, U, U, U, R 71, U, R 72, U, U, U, U, U, U, U, U],
                  [U, U, U, U, U, U, U, U, U, R 73, U, U, U, U, U, U, U, U, U]]

-- Hex
-- search for all positions around that satisfy certain requirements
findValidNeighbors :: [RowBoardState] -> Pos -> [Pos]
findValidNeighbors myBoard (x, y) = filter (testOccupyState myBoard) (findAllNeighbors (x, y))
-- search for all piece positions inside the board around
findAllNeighbors :: Pos -> [Pos]
findAllNeighbors (x, y) = filter testValid [(x-1, y-1), (x-1, y), (x-1, y+1), (x+1, y+1), (x+1, y), (x+1, y-01)]
-- test if a piece's position is out of boarder
testValid :: Pos -> Bool
testValid (x, y) = x >= 0 && y >= 0
-- access element in a matrix 
getElement :: [[a]] -> Pos -> a
getElement m (x, y) = (m !! y) !! x
-- test if a position is occupied by a piece
testOccupyState :: [[BoardType]] -> Pos -> Bool
testOccupyState myBoard (x, y) = isOccupied (getElement myBoard (x, y)) == Just False

testRedState :: [[BoardType]] -> Pos -> Bool
testRedState myBoard (x, y) = isRed (getElement myBoard (x, y))
-- an addition check should be transformed into square and tested

{-
    printBoard :: [[Int]] -> IO()
    printBoard [] = putStr ""
    printBoard (x:xs) = do print x
                        printBoard xs

    -- retrieve the occupy state of the board
    returnOccupiedBoard :: [RowBoardState] -> [[Int]]
    returnOccupiedBoard = map returnOccupiedRow
        where
            returnOccupiedRow :: RowBoardState -> [Int]
            returnOccupiedRow [] = []
            returnOccupiedRow (x:xs)
                | isNothing (isOccupied x) = (-1):returnOccupiedRow xs
                | isOccupied x == Just False = 0:returnOccupiedRow xs
                | otherwise = 1:returnOccupiedRow xs

    totalOccupied2 :: [[BoardType]] -> Int
    totalOccupied2 rs = length $ concatMap totalOccupied rs
        where
            totalOccupied :: [BoardType] -> [BoardType]
            totalOccupied [] = []
            totalOccupied (x:xs) = if isOccupied x == Just True then x:totalOccupied xs else totalOccupied xs
-}
-- the internal single-agent board for a player with the top-right the starting point and botton-left the destination
-- this is just for heuristic board evaluation 
-- only need to know the occupy status for each position
-- three main conversions are needed, others can be done through flipping
emptyList :: [[Int]]
emptyList = [
            [0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0]]

replace :: Int -> a -> [a] -> [a]
replace idx v xs = front ++ [v] ++ end
    where
        (front, _:end) = splitAt idx xs

-- project the red pieces from the main board to internal board
-- this is done through traversing the two boards
-- (3, 6)
moveUp :: Pos -> Pos
moveUp (x, y) = (x+1, y-1)

moveDown :: Pos -> Pos
moveDown (x, y) = (x+1, y+1)

projectToRow :: [RowBoardState] -> [Int] -> Pos -> [Int]
projectToRow _ [] _ = []
projectToRow eboard (x:xs) pos = if testRedState eboard pos then 1 : projectToRow eboard xs (moveUp pos)
                                 else x : projectToRow eboard xs (moveUp pos)

projectToWhole :: [RowBoardState] -> [[Int]] -> Pos -> [[Int]]
projectToWhole _ [] _ = []
projectToWhole eboard (x:xs) pos = projectToRow eboard x pos : projectToWhole eboard xs (moveDown pos)

flipBoard :: [[Int]] -> [[Int]]
flipBoard = transpose

printBoard :: [[Int]] -> IO()
printBoard [] = putStr ""
printBoard (x:xs) = do print x
                       printBoard xs