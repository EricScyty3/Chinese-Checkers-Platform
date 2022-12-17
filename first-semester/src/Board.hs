module Board where
import Data.Maybe
import Data.List

import Control.Monad.ST
import Data.STRef
-- import Control.Monad
import Control.Monad.State
import Control.Monad.Extra
import Control.Parallel

-- the board type should be able to represent the unique identification, occupy state and the occupied piece's color
data Colour = Green | Blue | Purple | Red | Orange | Black deriving (Eq, Show)
type Pos = (Int, Int)
data BoardType = G Pos | B Pos | P Pos | R Pos | O Pos | K Pos | E Pos | U Pos deriving (Eq, Show)
type Board = [[BoardType]]

-- the board setting for displaying
boardWidth :: Int
boardWidth = 19
boardHeight :: Int
boardHeight = 13
totalPieces :: Int
totalPieces = 6
occupiedBoardSize :: Int
occupiedBoardSize = 7

-- the colour list corresponding to each players with different amounts
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
compareColour btype colour = case getColour btype of
                                Just bcolour -> bcolour == colour
                                Nothing -> False

-- identify if a position on the board is empty or occupied
isEmpty :: BoardType -> Bool
isEmpty (E _) = True
isEmpty _ = False
isOccupied :: BoardType -> Bool
isOccupied btype = case getColour btype of
                    Just _ -> True
                    _      -> False
isSpacer :: BoardType -> Bool
isSpacer (U _) = True
isSpacer _ = False

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
safeRepaint :: Maybe Colour -> BoardType -> BoardType
safeRepaint Nothing b = b
safeRepaint (Just colour) b = repaint colour b
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
-- wrap the board as the state
getElement :: Pos -> State [[a]] a
getElement (x, y) = do n <- get
                       return ((n !! y) !! x)

-- replace version of 2d list
replace2 :: Pos -> a -> [[a]] -> [[a]]
replace2 (x, y) new ls = let row = replace x new (ls !! y)
                         in  replace y row ls

-- repalce a content with a new one in a list
replace :: Int -> a -> [a] -> [a]
replace idx new ls = front ++ [new] ++ end
    where
        (front, _:end) = splitAt idx ls

-- find the pieces' positions
findAllPieces :: Board -> [BoardType]
findAllPieces = concatMap (filter isOccupied)

-- find the pieces' positions on the board based on the colour
findPiecesWithColour :: Colour -> Board -> [BoardType]
findPiecesWithColour colour = concatMap (filter (`compareColour` colour))

-- additional applying ST monad to decrease the cost of copying the whole list         
-- update the new board with a modified piece
changeBoardElement :: (BoardType -> BoardType) -> BoardType -> Board -> Board
changeBoardElement f btype board = let new = f btype
                                       pos = getPos btype
                                   in  replace2 pos new board
-- erase the pieces on the board and keep certain coloured pieces, done at the initial state corresponding to the players amount
eraseBoard :: [Colour] -> Board
eraseBoard colourList = runST $ do n <- newSTRef externalBoard
                                   modifySTRef n (map (eraseRow colourList))
                                   readSTRef n
    where
        eraseRow :: [Colour] -> [BoardType] -> [BoardType]
        eraseRow _ [] = []
        eraseRow cs (x:xs) = case getColour x of
                                Nothing -> x:eraseRow cs xs
                                Just c  -> if c `notElem` cs then erase x:eraseRow cs xs
                                           else x:eraseRow cs xs
-- given two pieces, exchange their colours
repaintPath :: Board -> BoardType -> BoardType -> Board
repaintPath board start end  =  let colour = getColour start
                                    n1 = changeBoardElement erase start board
                                in  changeBoardElement (safeRepaint colour) end n1

destinationList :: BoardType -> State Board [BoardType]
destinationList btype = do case getColour btype of
                            Nothing -> return []
                            Just c  -> do adjacentMoves <- findAvaliableNeighbors btype
                                          chainedMoves  <- recursiveSearch [] btype
                                          let movesList = adjacentMoves `par` chainedMoves `pseq` nub (adjacentMoves ++ chainedMoves)
                                          return (filter (testCorners c) movesList)

-- state monad version
findAvaliableNeighbors :: BoardType -> State Board [BoardType]
findAvaliableNeighbors btype = do neighbourList <- mapM getElement neighborPosList
                                  return (filter isEmpty neighbourList)
    where
        (x, y) = getPos btype
        neighborPosList = filter (testValidPos boardWidth boardHeight)  [(x-1, y-1), (x-2, y), (x-1, y+1), (x+1, y+1), (x+2, y), (x+1, y-1)]

-- chained jump range
-- one over hop
-- recursively search for the reachable positions of multiple chained hops
-- state monad
recursiveSearch :: [BoardType] -> BoardType -> State Board [BoardType]
recursiveSearch ls btype = do chainJumpList <- jumpDirection (getPos btype)
                              let renewList = filter (`notElem` ls) chainJumpList
                                  recordList = renewList ++ ls
                              recursiveList <- concatMapM (recursiveSearch recordList) renewList
                              return (recursiveList ++ renewList)

-- state monad version 
jumpDirection :: Pos -> State Board [BoardType]
jumpDirection pos = do reachableList <- mapM (determineValidJump pos) [f (-1, -1), f (-2, 0), f (-1, 1), f (1, 1), f (2, 0), f (1, -1)]
                       let validReachableList = filter (/= pos) reachableList -- remove the invalid moves
                       mapM getElement validReachableList
    where
        f (a, b) (x, y) = (a+x, b+y)

determineValidJump :: Pos -> (Pos -> Pos) -> State Board Pos
determineValidJump pos f = do if not (testValidPos boardWidth boardHeight fp) ||
                                 not (testValidPos boardWidth boardHeight fp2) then return pos
                              else do n1 <- getElement fp
                                      n2 <- getElement fp2
                                      if isOccupied n1 && isEmpty n2 then return fp2
                                      else return pos
    where
        fp = f pos
        fp2 = (f . f) pos

-- test if a piece's position is out of boarder
testValidPos :: Int -> Int -> Pos -> Bool
testValidPos xlimt ylimt (x, y) = x >= 0 && y >= 0 && x < xlimt && y < ylimt

-- an addition check should be transformed into a square cooupied board and tested whether it exists in that board
-- this is only for computer players to enable sufficient compute and shorter game
testCorners :: Colour -> BoardType -> Bool
testCorners colour btype = testValidPos occupiedBoardSize occupiedBoardSize $ projection colour (getPos btype)

-- the projection of the main (display) board to the sub occupied board for each player
projection :: Colour -> Pos -> Pos
projection Green pos = projectGreen pos
projection Blue pos = projectBlue pos
projection Purple pos = projectPurple pos
projection Red pos = projectRed pos
projection Orange pos = projectOrange pos
projection Black pos = projectBlack pos
-- the projection of the sub occupied board of certain player to the main display board 
reversion :: Colour -> Pos -> Pos
reversion Green pos = reverseGreen pos
reversion Blue pos = reverseBlue pos
reversion Purple pos = reversePurple pos
reversion Red pos = reverseRed pos
reversion Orange pos = reverseOrange pos
reversion Black pos = reverseBlack pos
-- communication between different players' sub-boards
-- first reverse to the main board coordinate system then convert to the player's occupied board system
conversion :: Pos -> Colour -> Colour -> Pos
conversion p c1 c2 = projection c2 (reversion c1 p)
-- Below is the coordinate conversion of internal board state of each colour and the external display board state
{-
        (1,1)
        (6,9) (7,8) (8,7) (9,6)
        (5,8) (6,7) (7,6) (8,5)
        (4,7) (5,6) (6,5) (7,4)
(1,-1)  (3,6) (4,5) (5,4) (6,3)
-}
projectGreen :: Pos -> Pos
projectGreen (x, y) = let disY = ((x - ix) + (y - iy)) `div` 2
                          (ox, oy) = (ix + disY, iy + disY) -- move Y
                      in  (x - ox, disY)
    where
        (ix, iy) = (3, 6)

reverseGreen :: Pos -> Pos
reverseGreen (x,y) = let (ox, oy) = (ix + x, iy - x) -- move x
                     in  (ox + y, oy + y)            -- move y
    where
        (ix, iy) = (3, 6)
{-
         (2, 0)
         (12,9) (11,8) (10,7) (9, 6)
         (10,9) (9, 8) (8, 7) (7, 6)
         (8, 9) (7, 8) (6, 7) (5, 6)
(-1,-1)  (6, 9) (5, 8) (4, 7) (3, 6)
-}
projectBlue :: Pos -> Pos
projectBlue (x, y) = let disX = iy - y
                         (ox, oy) = (ix - disX, iy - disX) -- move x
                     in  (disX, (x - ox) `div` 2)
    where
        (ix, iy) = (6, 9)

reverseBlue :: Pos -> Pos
reverseBlue (x, y) = let (ox, oy) = (ix - x, iy - x) -- move x
                     in  (2*y + ox, oy)              -- move y
    where
        (ix, iy) = (6, 9)
{-
        (1,-1)
        (15,6) (13,6) (11,6) (9, 6)
        (14,7) (12,7) (10,7) (8, 7)
        (13,8) (11,8) (9, 8) (7, 8)
(-2,0)  (12,9) (10,9) (8, 9) (6, 9)
-}
projectPurple :: Pos -> Pos
projectPurple (x, y) = let disY = iy - y
                           (ox, oy) = (ix + disY, iy - disY) -- move y
                       in  ((ox-x) `div` 2, disY)
    where
        (ix, iy) = (12, 9)

reversePurple :: Pos -> Pos
reversePurple (x, y) = let (ox, oy) = (ix - 2*x, iy) -- move x
                       in  (ox + y, oy - y)          -- move y
    where
        (ix, iy) = (12, 9)
{-
        (-1,-1)
        (12,3) (11,4) (10,5) (9, 6)
        (13,4) (12,5) (11,6) (10,7)
        (14,5) (13,6) (12,7) (11,8)
(-1,1)  (15,6) (14,7) (13,8) (12,9)
-}
projectRed :: Pos -> Pos
projectRed (x, y) = let disY = ((ix - x) + (iy - y)) `div` 2
                        (ox, oy) = (ix - disY, iy - disY) -- move y
                    in  (ox - x, disY)
    where
        (ix, iy) = (15, 6)

reverseRed :: Pos -> Pos
reverseRed (x, y) = let (ox, oy) = (ix - x, iy + x) -- move x
                    in  (ox - y, oy - y)            -- move y
    where
        (ix, iy) = (15, 6)
{-
        (-2,0)
        (6, 3) (7, 4) (8, 5) (9, 6)
        (8, 3) (9, 4) (10,5) (11,6)
        (10,3) (11,4) (12,5) (13,6)
(1, 1)  (12,3) (13,4) (14,5) (15,6)
-}
projectOrange :: Pos -> Pos
projectOrange (x, y) = let disX = y - iy
                           (ox, oy) = (ix + disX, iy + disX) -- move x 
                       in  (disX, (ox - x) `div` 2)
    where
        (ix, iy) = (12, 3)

reverseOrange :: Pos -> Pos
reverseOrange (x, y) = let (ox, oy) = (ix + x, iy + x) -- move x
                       in  (ox - 2*y, oy)              -- move y
    where
        (ix, iy) = (12, 3)
{-
        (-1,1)
        (3, 6) (5, 6) (7, 6) (9, 6)
        (4, 5) (6, 5) (8, 5) (10,5)
        (5, 4) (7, 4) (9, 4) (11,4)
(2, 0)  (6, 3) (8, 3) (10,3) (12,3)
-}
projectBlack :: Pos -> Pos
projectBlack (x, y) = let disY = y - iy
                          (ox, oy) = (ix - disY, iy + disY) -- move Y
                      in  ((x-ox) `div` 2, disY)
    where
        (ix, iy) = (6, 3)

reverseBlack :: Pos -> Pos
reverseBlack (x, y) = let (ox, oy) = (ix + 2 * x, iy) -- move x
                      in  (ox - y, oy + y)            -- move y
    where
        (ix, iy) = (6, 3)

-- the board printing for occupied board
printBoard :: Show a => [a] -> IO ()
printBoard [] = putStr ""
printBoard (x:xs) = do print x
                       printBoard xs

-- the board printing for the main board
printEoard :: Board -> IO ()
printEoard b = printEoard' $ testDisplay b
    where
        printEoard' :: Show a => [a] -> IO ()
        printEoard' [] = putStrLn ""
        printEoard' (x:xs) = do let str = map skipZero (show x)
                                putStrLn str
                                printEoard' xs

        skipZero :: Char -> Char
        skipZero '0' = ' '
        skipZero ',' = ' '
        skipZero a = a

-- transform the board types into numerical values
testDisplay :: Board -> [[Int]]
testDisplay = map testDisplay'
    where
        testDisplay' :: [BoardType] -> [Int]
        testDisplay' [] = []
        testDisplay' (x:xs) = case getColour x of
                                Just c -> colourToIndex c + 1:testDisplay' xs
                                _ -> 0:testDisplay' xs

        colourToIndex :: Colour -> Int
        colourToIndex colour = fromMaybe 0 (elemIndex colour sixPlayersSet)
