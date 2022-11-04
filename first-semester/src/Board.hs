module Board where
import Data.Maybe
import Data.List

-- the board type should be able to represent the unique identification, occupy state and the occupied piece's color
data BoardType = G Int | B Int | P Int | R Int | O Int | Y Int | E Int | U deriving (Eq, Show)
data Color = Green | Blue | Purple | Red | Orange | Yellow deriving (Eq, Show)
type RowBoardState = [BoardType]
type Pos = (Int, Int)
boardWidth :: Int
boardWidth = 19
boardHeight :: Int
boardHeight = 13
totalPieces :: Int
totalPieces = 6

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
isOrange :: BoardType -> Bool
isOrange (O _) = True
isOrange _ = False
isPurple :: BoardType -> Bool
isPurple (P _) = True
isPurple _ = False
isYellow :: BoardType -> Bool
isYellow (Y _) = True
isYellow _ = False
-- identify if a position on the board is occupied
isOccupied :: BoardType -> Maybe Bool
isOccupied U = Nothing
isOccupied (E _) = Just False
isOccupied _ = Just True
-- get the assoicated id for the piece
getIndex :: BoardType -> Int
getIndex U = -1
getIndex (Y x) = x
getIndex (O x) = x
getIndex (P x) = x
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
                  [U, U, U, U, U, U, U, U, U, G 1, U, U, U, U, U, U, U, U, U],
                  [U, U, U, U, U, U, U, U, G 2, U, G 3, U, U, U, U, U, U, U, U],
                  [U, U, U, U, U, U, U, G 4, U, G 5, U, G 6 , U, U, U, U, U, U, U],
                  [B 7, U, B 8, U, B 9, U, E 10, U, E 11, U, E 12, U, E 13, U, Y 14, U, Y 15, U, Y 16],
                  [U, B 17, U, B 18, U, E 19, U, E 20, U, E 21, U, E 22, U, E 23, U, Y 24, U, Y 25, U],
                  [U, U, B 26, U, E 27, U, E 28, U, E 29, U, E 30, U, E 31, U, E 32, U, Y 33, U, U],
                  [U, U, U, E 34, U, E 35, U, E 36, U, E 37, U, E 38, U, E 39, U, E 40, U, U, U],
                  [U, U, P 41, U, E 42, U, E 43, U, E 44, U, E 45, U, E 46, U, E 47, U, O 48, U, U],
                  [U, P 49, U, P 50, U, E 51, U, E 52, U, E 53, U, E 54, U, E 55, U, O 56, U, O 57, U],
                  [P 58, U, P 59, U, P 60, U, E 61, U, E 62, U, E 63, U, E 64, U, O 65, U, O 66, U, O 67],
                  [U, U, U, U, U, U, U, R 68, U, R 69, U, R 70, U, U, U, U, U, U, U],
                  [U, U, U, U, U, U, U, U, R 71, U, R 72, U, U, U, U, U, U, U, U],
                  [U, U, U, U, U, U, U, U, U, R 73, U, U, U, U, U, U, U, U, U]]
{-
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
-}
-- access element in a matrix 
getElement :: [[a]] -> Pos -> a
getElement m (x, y) = (m !! y) !! x
-- test if a position is occupied by a piece
testOccupyState :: [[BoardType]] -> Pos -> Bool
testOccupyState myBoard (x, y) = isOccupied (getElement myBoard (x, y)) == Just False
-- check the color of a certain position 
testRedState :: [[BoardType]] -> Pos -> Bool
testRedState myBoard (x, y) = isRed (getElement myBoard (x, y))
testBlueState :: [[BoardType]] -> Pos -> Bool
testBlueState myBoard (x, y) = isBlue (getElement myBoard (x, y))
testGreenState :: [[BoardType]] -> Pos -> Bool
testGreenState myBoard (x, y) = isGreen (getElement myBoard (x, y))
testPurpleState :: [[BoardType]] -> Pos -> Bool
testPurpleState myBoard (x, y) = isPurple (getElement myBoard (x, y))
testOrangeState :: [[BoardType]] -> Pos -> Bool
testOrangeState myBoard (x, y) = isOrange (getElement myBoard (x, y))
testYellowState :: [[BoardType]] -> Pos -> Bool
testYellowState myBoard (x, y) = isYellow (getElement myBoard (x, y))
-- an addition check should be transformed into square and tested
-- through counting the colored pieces
testValidMove :: [RowBoardState] -> Color -> Bool
testValidMove resultBoard color = sum (map sum (project resultBoard color)) == totalPieces

{-
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

    replace :: Int -> a -> [a] -> [a]
    replace idx v xs = front ++ [v] ++ end
        where
            (front, _:end) = splitAt idx xs
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

-- for testing purpose
getPosByIdx :: Int -> Int -> Int -> (Int, Int)
getPosByIdx v x y
  | getIndex (getElement externalBoard (x, y)) == v = (x, y)
  | x == boardWidth - 1 = getPosByIdx v 0 (y+1)
  | otherwise = getPosByIdx v (x+1) y
-- project the red pieces from the main board to internal board
-- this is done through traversing the two boards
project :: [RowBoardState] -> Color -> [[Int]]
project [] _ = []
project myBoard Green  = projectToGreen myBoard emptyList (3, 6)
project myBoard Blue   = projectToBlue myBoard emptyList (6, 9)
project myBoard Purple = projectToPurple myBoard emptyList (12, 9)
project myBoard Red    = projectToRed myBoard emptyList (15, 6)
project myBoard Orange = projectToOrange myBoard emptyList (12, 3)
project myBoard Yellow = projectToYellow myBoard emptyList (6, 3)

projectToGreen :: [RowBoardState] -> [[Int]] -> Pos -> [[Int]]
projectToGreen _ [] _ = []
projectToGreen eboard (x:xs) pos = projectToRowGreen eboard x pos : projectToGreen eboard xs (moveDown pos)
    where
        projectToRowGreen :: [RowBoardState] -> [Int] -> Pos -> [Int]
        projectToRowGreen _ [] _ = []
        projectToRowGreen eboard (x:xs) pos = if testGreenState eboard pos then 1 : projectToRowGreen eboard xs (moveUp pos)
                                              else x : projectToRowGreen eboard xs (moveUp pos)
        moveUp :: Pos -> Pos
        moveUp (x, y) = (x+1, y-1)
        moveDown :: Pos -> Pos
        moveDown (x, y) = (x+1, y+1)

projectToBlue :: [RowBoardState] -> [[Int]] -> Pos -> [[Int]]
projectToBlue _ [] _ = []
projectToBlue eboard (x:xs) pos = projectToRowBlue eboard x pos : projectToBlue eboard xs (moveRight pos)
    where
        projectToRowBlue :: [RowBoardState] -> [Int] -> Pos -> [Int]
        projectToRowBlue _ [] _ = []
        projectToRowBlue eboard (x:xs) pos = if testBlueState eboard pos then 1 : projectToRowBlue eboard xs (moveUp pos)
                                             else x : projectToRowBlue eboard xs (moveUp pos)
        moveUp :: Pos -> Pos
        moveUp (x, y) = (x-1, y-1)
        moveRight :: Pos -> Pos
        moveRight (x, y) = (x+2, y)

projectToPurple :: [RowBoardState] -> [[Int]] -> Pos -> [[Int]]
projectToPurple _ [] _ = []
projectToPurple eboard (x:xs) pos = projectToRowPurple eboard x pos : projectToPurple eboard xs (moveUp pos)
    where
        projectToRowPurple :: [RowBoardState] -> [Int] -> Pos -> [Int]
        projectToRowPurple _ [] _ = []
        projectToRowPurple eboard (x:xs) pos = if testPurpleState eboard pos then 1 : projectToRowPurple eboard xs (moveLeft pos)
                                               else x : projectToRowPurple eboard xs (moveLeft pos)
        moveUp :: Pos -> Pos
        moveUp (x, y) = (x+1, y-1)
        moveLeft :: Pos -> Pos
        moveLeft (x, y) = (x-2, y)

projectToRed :: [RowBoardState] -> [[Int]] -> Pos -> [[Int]]
projectToRed _ [] _ = []
projectToRed eboard (x:xs) pos = projectToRowRed eboard x pos : projectToRed eboard xs (moveUp pos)
    where
        projectToRowRed :: [RowBoardState] -> [Int] -> Pos -> [Int]
        projectToRowRed _ [] _ = []
        projectToRowRed eboard (x:xs) pos = if testRedState eboard pos then 1 : projectToRowRed eboard xs (moveDown pos)
                                               else x : projectToRowRed eboard xs (moveDown pos)
        moveDown :: Pos -> Pos
        moveDown (x, y) = (x-1, y+1)
        moveUp :: Pos -> Pos
        moveUp (x, y) = (x-1, y-1)

projectToOrange :: [RowBoardState] -> [[Int]] -> Pos -> [[Int]]
projectToOrange _ [] _ = []
projectToOrange eboard (x:xs) pos = projectToRowOrange eboard x pos : projectToOrange eboard xs (moveLeft pos)
    where
        projectToRowOrange :: [RowBoardState] -> [Int] -> Pos -> [Int]
        projectToRowOrange _ [] _ = []
        projectToRowOrange eboard (x:xs) pos = if testOrangeState eboard pos then 1 : projectToRowOrange eboard xs (moveDown pos)
                                               else x : projectToRowOrange eboard xs (moveDown pos)
        moveDown :: Pos -> Pos
        moveDown (x, y) = (x+1, y+1)
        moveLeft :: Pos -> Pos
        moveLeft (x, y) = (x-2, y)

projectToYellow :: [RowBoardState] -> [[Int]] -> Pos -> [[Int]]
projectToYellow _ [] _ = []
projectToYellow eboard (x:xs) pos = projectToRowYellow eboard x pos : projectToYellow eboard xs (moveDown pos)
    where
        projectToRowYellow :: [RowBoardState] -> [Int] -> Pos -> [Int]
        projectToRowYellow _ [] _ = []
        projectToRowYellow eboard (x:xs) pos = if testYellowState eboard pos then 1 : projectToRowYellow eboard xs (moveRight pos)
                                               else x : projectToRowYellow eboard xs (moveRight pos)
        moveRight :: Pos -> Pos
        moveRight (x, y) = (x+2, y)
        moveDown :: Pos -> Pos
        moveDown (x, y) = (x-1, y+1)

-- flipBoard :: [[Int]] -> [[Int]]
-- flipBoard = transpose

printBoard :: [[Int]] -> IO()
printBoard [] = putStr ""
printBoard (x:xs) = do print x
                       printBoard xs