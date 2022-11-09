module Board where
import Data.Maybe
import Data.List

-- the board type should be able to represent the unique identification, occupy state and the occupied piece's color
data Colour = Green | Blue | Purple | Red | Orange | Black deriving (Eq, Show)
type Pos = (Int, Int)
data BoardType = G Pos | B Pos | P Pos | R Pos | O Pos | K Pos | E Pos | U Pos deriving (Eq, Show)
type Board = [[BoardType]]

boardWidth :: Int
boardWidth = 19
boardHeight :: Int
boardHeight = 13
totalPieces :: Int
totalPieces = 6
twoPlayersSet :: [Colour]
twoPlayersSet = [Green, Red]
threePlayersSet :: [Colour]
threePlayersSet = [Green, Purple, Orange]
fourPlayersSet :: [Colour]
fourPlayersSet = [Blue, Purple, Orange, Black]
sixPlayersSet :: [Colour]
sixPlayersSet = [Green, Blue, Purple, Red, Orange, Black]
-- determine the color state of a piece
isRed :: BoardType -> Bool
isRed (R _) = True
isRed _ = False
isGreen :: BoardType -> Bool
isGreen (G _) = True
isGreen _ = False
isBlue :: BoardType -> Bool
isBlue (B _) = True
isBlue _ = False
isOrange :: BoardType -> Bool
isOrange (O _) = True
isOrange _ = False
isPurple :: BoardType -> Bool
isPurple (P _) = True
isPurple _ = False
isBlack :: BoardType -> Bool
isBlack (K _) = True
isBlack _ = False
getColour :: BoardType -> Maybe Colour
getColour (G _) = Just Green
getColour (B _) = Just Blue
getColour (P _) = Just Purple
getColour (R _) = Just Red
getColour (O _) = Just Orange
getColour (K _) = Just Black
getColour _ = Nothing
compareColour :: BoardType -> Colour -> Bool
compareColour b c = case getColour b of
                        Just bc -> bc == c
                        Nothing -> False
-- identify if a position on the board is occupied
isOccupied :: BoardType -> Maybe Bool
isOccupied (U _) = Nothing
isOccupied (E _) = Just False
isOccupied _ = Just True
-- get the assoicated position for the piece
getPos :: BoardType -> Pos
getPos (U p) = p
getPos (K p) = p
getPos (O p) = p
getPos (P p) = p
getPos (R p) = p
getPos (B p) = p
getPos (G p) = p
getPos (E p) = p
-- update the new colour state for a board position
repaint :: Colour -> BoardType -> BoardType
repaint Green b = G (getPos b)
repaint Black b = K (getPos b)
repaint Orange b = O (getPos b)
repaint Red b = R (getPos b)
repaint Blue b = B (getPos b)
repaint Purple b = P (getPos b)
erase :: BoardType -> BoardType
erase b = E (getPos b)
-- the overall board state for displaying and for movement changed determining
{-
    externalBoard :: Board
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
                    [U, U, U, U, U, U, U, U, U, R 73, U, U, U, U, U, U, U, U, U]
                ]
    transform :: [[BoardType]] -> Int -> IO ()
    transform [] _ = putStrLn ""
    transform [x] iy = printExternalBoardWithPos x 0 iy
    transform (x:xs) iy = do printExternalBoardWithPos x 0 iy
                            transform xs (iy+1)

    printExternalBoardWithPos :: [BoardType] -> Int -> Int -> IO ()
    printExternalBoardWithPos [] _ _ = putStrLn ""
    printExternalBoardWithPos [x] ix fy = putStrLn (show x ++ "(" ++ show ix ++ ", " ++ show fy ++ ")")
    printExternalBoardWithPos (x:xs) ix fy = do case isOccupied x of 
                                                    Nothing -> putStr (show x ++ "(" ++ show ix ++ ", " ++ show fy ++ "), ")
                                                    _       -> putStr (show x ++ " (" ++ show ix ++ ", " ++ show fy ++ "), ")
                                                printExternalBoardWithPos xs (ix+1) fy
-}
externalBoard :: Board
externalBoard = [
    [U(0, 0), U(1, 0), U(2, 0), U(3, 0), U(4, 0), U(5, 0), U(6, 0), U(7, 0), U(8, 0), G(9, 0), U(10, 0), U(11, 0), U(12, 0), U(13, 0), U(14, 0), U(15, 0), U(16, 0), U(17, 0), U(18, 0)],
    [U(0, 1), U(1, 1), U(2, 1), U(3, 1), U(4, 1), U(5, 1), U(6, 1), U(7, 1), G(8, 1), U(9, 1), G(10, 1), U(11, 1), U(12, 1), U(13, 1), U(14, 1), U(15, 1), U(16, 1), U(17, 1), U(18, 1)],
    [U(0, 2), U(1, 2), U(2, 2), U(3, 2), U(4, 2), U(5, 2), U(6, 2), G(7, 2), U(8, 2), G(9, 2), U(10, 2), G(11, 2), U(12, 2), U(13, 2), U(14, 2), U(15, 2), U(16, 2), U(17, 2), U(18, 2)],
    [B(0, 3), U(1, 3), B(2, 3), U(3, 3), B(4, 3), U(5, 3), E(6, 3), U(7, 3), E(8, 3), U(9, 3), E(10, 3), U(11, 3), E(12, 3), U(13, 3), K(14, 3), U(15, 3), K(16, 3), U(17, 3), K(18, 3)],
    [U(0, 4), B(1, 4), U(2, 4), B(3, 4), U(4, 4), E(5, 4), U(6, 4), E(7, 4), U(8, 4), E(9, 4), U(10, 4), E(11, 4), U(12, 4), E(13, 4), U(14, 4), K(15, 4), U(16, 4), K(17, 4), U(18, 4)],
    [U(0, 5), U(1, 5), B(2, 5), U(3, 5), E(4, 5), U(5, 5), E(6, 5), U(7, 5), E(8, 5), U(9, 5), E(10, 5), U(11, 5), E(12, 5), U(13, 5), E(14, 5), U(15, 5), K(16, 5), U(17, 5), U(18, 5)],
    [U(0, 6), U(1, 6), U(2, 6), E(3, 6), U(4, 6), E(5, 6), U(6, 6), E(7, 6), U(8, 6), E(9, 6), U(10, 6), E(11, 6), U(12, 6), E(13, 6), U(14, 6), E(15, 6), U(16, 6), U(17, 6), U(18, 6)],
    [U(0, 7), U(1, 7), P(2, 7), U(3, 7), E(4, 7), U(5, 7), E(6, 7), U(7, 7), E(8, 7), U(9, 7), E(10, 7), U(11, 7), E(12, 7), U(13, 7), E(14, 7), U(15, 7), O(16, 7), U(17, 7), U(18, 7)],
    [U(0, 8), P(1, 8), U(2, 8), P(3, 8), U(4, 8), E(5, 8), U(6, 8), E(7, 8), U(8, 8), E(9, 8), U(10, 8), E(11, 8), U(12, 8), E(13, 8), U(14, 8), O(15, 8), U(16, 8), O(17, 8), U(18, 8)],
    [P(0, 9), U(1, 9), P(2, 9), U(3, 9), P(4, 9), U(5, 9), E(6, 9), U(7, 9), E(8, 9), U(9, 9), E(10, 9), U(11, 9), E(12, 9), U(13, 9), O(14, 9), U(15, 9), O(16, 9), U(17, 9), O(18, 9)],
    [U(0, 10), U(1, 10), U(2, 10), U(3, 10), U(4, 10), U(5, 10), U(6, 10), R(7, 10), U(8, 10), R(9, 10), U(10, 10), R(11, 10), U(12, 10), U(13, 10), U(14, 10), U(15, 10), U(16, 10), U(17, 10), U(18, 10)],
    [U(0, 11), U(1, 11), U(2, 11), U(3, 11), U(4, 11), U(5, 11), U(6, 11), U(7, 11), R(8, 11), U(9, 11), R(10, 11), U(11, 11), U(12, 11), U(13, 11), U(14, 11), U(15, 11), U(16, 11), U(17, 11), U(18, 11)],
    [U(0, 12), U(1, 12), U(2, 12), U(3, 12), U(4, 12), U(5, 12), U(6, 12), U(7, 12), U(8, 12), R(9, 12), U(10, 12), U(11, 12), U(12, 12), U(13, 12), U(14, 12), U(15, 12), U(16, 12), U(17, 12), U(18, 12)]]

-- access element in a matrix 
getElement :: Board -> Pos -> BoardType
getElement m (x, y) = m !! y !! x
-- repalce a content with a new one
replace :: Int -> a -> [a] -> [a]
replace idx v xs = front ++ [v] ++ end
    where
        (front, _:end) = splitAt idx xs
-- update the new board with modifications
changeBoardElement :: (BoardType -> BoardType) -> BoardType -> Board -> Board
changeBoardElement f bt myBoard = let newElement = f bt
                                      (x, y) = getPos newElement
                                      newRow = replace x newElement (myBoard !! y)
                                  in  replace y newRow myBoard

isJustFalse :: Maybe Bool -> Bool
isJustFalse (Just False) = True
isJustFalse _ = False

isJustTrue :: Maybe Bool -> Bool
isJustTrue (Just True) = True
isJustTrue _ = False

-- erase the board sections according to the colours, whether to keep (True) or remove (False)
eraseBoard :: Bool -> [Colour] -> Board -> Board
eraseBoard t cs = map (eraseRow t cs)
    where
        eraseRow :: Bool -> [Colour] -> [BoardType] -> [BoardType]
        eraseRow t _ [] = []
        eraseRow t cs (x:xs) = case getColour x of
                                Nothing -> x:eraseRow t cs xs
                                Just c  -> if f t c cs then erase x:eraseRow t cs xs
                                           else x:eraseRow t cs xs

        f :: Bool -> Colour -> [Colour] -> Bool
        f t x xs = if t then x `elem` xs else x `notElem` xs

repaintPath :: BoardType -> BoardType -> Board -> Board
repaintPath start end myBoard = let startColour = getColour start
                                in  case startColour of
                                        Nothing -> myBoard
                                        Just c  -> let tempBoard = changeBoardElement erase start myBoard
                                                   in  changeBoardElement (repaint c) end tempBoard

-- testJumpValid :: Board -> BoardType -> BoardType -> Bool
-- testJumpValid eBoard start end = end `elem` destinationList eBoard start

destinationList :: Board -> BoardType -> [BoardType]
destinationList eBoard b = nub $ findAvaliableNeighbors eBoard b ++ searchWithoutLooping eBoard [] b

-- check if this can be reached by one jump
-- One adjacent jump range
-- search for all neighbor positions around that are not occupied
findAvaliableNeighbors :: Board -> BoardType -> [BoardType]
findAvaliableNeighbors myBoard b = filter (isJustFalse . isOccupied) (findValidNeighbors (getPos b) myBoard)

-- findTureNeighbors :: Pos -> Board -> Bool
-- findTureNeighbors :: [BoardType] -> [BoardType]
-- findTureNeighbors = filter (isJust . isOccupied)
-- search for all piece positions inside the board around
findValidNeighbors :: Pos -> Board -> [BoardType]
findValidNeighbors (x, y) myBoard = map (getElement myBoard) (filter testValidPos [(x-1, y-1), (x-2, y), (x-1, y+1), (x+1, y+1), (x+2, y), (x+1, y-1)])
-- test if a piece's position is out of boarder
testValidPos :: Pos -> Bool
testValidPos (x, y) = (x >= 0 && y >= 0) && x <= boardWidth - 1 && y <= boardHeight - 1

-- chained jump range
-- one over hop
searchWithoutLooping :: Board -> [BoardType] -> BoardType ->  [BoardType]
searchWithoutLooping myBoard l b = let s = recursiveSearch myBoard b
                                       renewList = filter (`notElem` l) s
                                       recordList = renewList ++ l
                                   in  concatMap (searchWithoutLooping myBoard recordList) renewList ++ renewList

recursiveSearch :: Board -> BoardType -> [BoardType]
recursiveSearch eBoard b = map (getElement eBoard) (jumpToAllDirections eBoard (getPos b))
-- one piece dor all six driections checked
jumpToAllDirections :: Board -> Pos -> [Pos]
jumpToAllDirections myBoard pos = filter (/= pos) (jumpToOneDirection myBoard pos [(-1, -1), (-2, 0), (-1, 1), (1, 1), (2, 0), (1, -1)])

jumpToOneDirection :: Board -> Pos -> [Pos] -> [Pos]
jumpToOneDirection _ _ [] = []
jumpToOneDirection eBoard pos (a:as) = determineValidJump eBoard pos (f a): jumpToOneDirection eBoard pos as
    where
        f (a, b) (x, y) = (a+x, b+y)

determineValidJump :: Board -> Pos -> (Pos -> Pos) -> Pos
determineValidJump myBoard pos f
    | not (testValidPos (f pos)) || not (testValidPos ((f . f) pos)) = pos -- invalid ones
    | isJustTrue (isOccupied $ getElement myBoard $ f pos) &&
      isJustFalse (isOccupied $ getElement myBoard $ (f . f) pos) = (f . f) pos
    | otherwise = pos -- no ways found

-- some rules allow jump to be chained as longer as the symmetric satisfied on a line, no neec to be one position in between

-- an addition check should be transformed into square and tested
-- through counting the colored pieces, this is only for computer players to enable sufficient compute and shorter game
testCorners :: Board -> Colour -> Bool
testCorners resultBoard color = sum (map sum (project resultBoard color)) == totalPieces
-- bedies, computer is only allow frontward move

-- a win for a player can only be achieved at one's turn
winStateDetect :: Board -> Colour -> Board -> Bool
winStateDetect eBoard c iBoard = let projection1 = project eBoard c
                                     projection2 = project iBoard c
                                 in  flipBoard projection2 == projection1
{-
    -- retrieve the occupy state of the board
    returnOccupiedBoard :: Board -> [[Int]]
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
-- getPosByIdx :: Int -> Int -> Int -> Pos
-- getPosByIdx v x y
--   | getIndex (getElement externalBoard (x, y)) == v = (x, y)
--   | x == boardWidth - 1 = getPosByIdx v 0 (y+1)
--   | otherwise = getPosByIdx v (x+1) y
-- project the red pieces from the main board to internal board
-- this is done through traversing the two boards
project :: Board -> Colour -> [[Int]]
project [] _ = []
project myBoard Green  = projectToGreen myBoard emptyList (3, 6)
project myBoard Blue   = projectToBlue myBoard emptyList (6, 9)
project myBoard Purple = projectToPurple myBoard emptyList (12, 9)
project myBoard Red    = projectToRed myBoard emptyList (15, 6)
project myBoard Orange = projectToOrange myBoard emptyList (12, 3)
project myBoard Black = projectToBlack myBoard emptyList (6, 3)

testColour :: Pos -> Board -> Maybe Colour
testColour pos myBoard = getColour $ getElement myBoard pos

projectToGreen :: Board -> [[Int]] -> Pos -> [[Int]]
projectToGreen _ [] _ = []
projectToGreen eboard (x:xs) pos = projectToRowGreen eboard x pos : projectToGreen eboard xs (moveDown pos)
    where
        projectToRowGreen :: Board -> [Int] -> Pos -> [Int]
        projectToRowGreen _ [] _ = []
        projectToRowGreen eboard (x:xs) pos = if testColour pos eboard == Just Green then 1 : projectToRowGreen eboard xs (moveUp pos)
                                              else x : projectToRowGreen eboard xs (moveUp pos)
        moveUp :: Pos -> Pos
        moveUp (x, y) = (x+1, y-1)
        moveDown :: Pos -> Pos
        moveDown (x, y) = (x+1, y+1)

projectToBlue :: Board -> [[Int]] -> Pos -> [[Int]]
projectToBlue _ [] _ = []
projectToBlue eboard (x:xs) pos = projectToRowBlue eboard x pos : projectToBlue eboard xs (moveRight pos)
    where
        projectToRowBlue :: Board -> [Int] -> Pos -> [Int]
        projectToRowBlue _ [] _ = []
        projectToRowBlue eboard (x:xs) pos = if testColour pos eboard == Just Blue then 1 : projectToRowBlue eboard xs (moveUp pos)
                                             else x : projectToRowBlue eboard xs (moveUp pos)
        moveUp :: Pos -> Pos
        moveUp (x, y) = (x-1, y-1)
        moveRight :: Pos -> Pos
        moveRight (x, y) = (x+2, y)

projectToPurple :: Board -> [[Int]] -> Pos -> [[Int]]
projectToPurple _ [] _ = []
projectToPurple eboard (x:xs) pos = projectToRowPurple eboard x pos : projectToPurple eboard xs (moveUp pos)
    where
        projectToRowPurple :: Board -> [Int] -> Pos -> [Int]
        projectToRowPurple _ [] _ = []
        projectToRowPurple eboard (x:xs) pos = if testColour pos eboard == Just Purple then 1 : projectToRowPurple eboard xs (moveLeft pos)
                                               else x : projectToRowPurple eboard xs (moveLeft pos)
        moveUp :: Pos -> Pos
        moveUp (x, y) = (x+1, y-1)
        moveLeft :: Pos -> Pos
        moveLeft (x, y) = (x-2, y)

projectToRed :: Board -> [[Int]] -> Pos -> [[Int]]
projectToRed _ [] _ = []
projectToRed eboard (x:xs) pos = projectToRowRed eboard x pos : projectToRed eboard xs (moveUp pos)
    where
        projectToRowRed :: Board -> [Int] -> Pos -> [Int]
        projectToRowRed _ [] _ = []
        projectToRowRed eboard (x:xs) pos = if testColour pos eboard == Just Red then 1 : projectToRowRed eboard xs (moveDown pos)
                                               else x : projectToRowRed eboard xs (moveDown pos)
        moveDown :: Pos -> Pos
        moveDown (x, y) = (x-1, y+1)
        moveUp :: Pos -> Pos
        moveUp (x, y) = (x-1, y-1)

projectToOrange :: Board -> [[Int]] -> Pos -> [[Int]]
projectToOrange _ [] _ = []
projectToOrange eboard (x:xs) pos = projectToRowOrange eboard x pos : projectToOrange eboard xs (moveLeft pos)
    where
        projectToRowOrange :: Board -> [Int] -> Pos -> [Int]
        projectToRowOrange _ [] _ = []
        projectToRowOrange eboard (x:xs) pos = if testColour pos eboard == Just Orange then 1 : projectToRowOrange eboard xs (moveDown pos)
                                               else x : projectToRowOrange eboard xs (moveDown pos)
        moveDown :: Pos -> Pos
        moveDown (x, y) = (x+1, y+1)
        moveLeft :: Pos -> Pos
        moveLeft (x, y) = (x-2, y)

projectToBlack :: Board -> [[Int]] -> Pos -> [[Int]]
projectToBlack _ [] _ = []
projectToBlack eboard (x:xs) pos = projectToRowBlack eboard x pos : projectToBlack eboard xs (moveDown pos)
    where
        projectToRowBlack :: Board -> [Int] -> Pos -> [Int]
        projectToRowBlack _ [] _ = []
        projectToRowBlack eboard (x:xs) pos = if testColour pos eboard == Just Black then 1 : projectToRowBlack eboard xs (moveRight pos)
                                               else x : projectToRowBlack eboard xs (moveRight pos)
        moveRight :: Pos -> Pos
        moveRight (x, y) = (x+2, y)
        moveDown :: Pos -> Pos
        moveDown (x, y) = (x-1, y+1)

flipBoard :: [[Int]] -> [[Int]]
flipBoard = transpose

printBoard :: Show a => [a] -> IO ()
printBoard [] = putStr ""
printBoard (x:xs) = do print x
                       printBoard xs
