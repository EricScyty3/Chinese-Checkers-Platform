module Board where
-- The operator of displaying board state
import Data.Maybe
import Data.List
import Control.Monad.ST
import Data.STRef
import Control.Monad.State
import Control.Monad.Extra
import Control.Parallel

-- the board position should be able to represent the occupy state and the occupied piece's color
data Colour = Green | Blue | Purple | Red | Orange | Black deriving (Eq, Show)
type Pos = (Int, Int)
data BoardPos = G Pos | B Pos | P Pos | R Pos | O Pos | K Pos | E Pos | U Pos deriving (Eq, Show)
type Board = [[BoardPos]]
type OccupiedBoard = [[Int]] -- for occupied board, only occupied state is needed



-- the board setting for displaying
boardWidth :: Int
boardWidth = 19
boardHeight :: Int
boardHeight = 13
-- the total pieces a player would need to play with
totalPieces :: Int
totalPieces = 6
-- the star-shape board could also projected to a square board corresponding to different piece's colour
occupiedBoardSize :: Int
occupiedBoardSize = 7

-- the colour corresponding to each player with different number of players allowed
playerColourList :: Int -> [Colour]
playerColourList 2 = [Green, Red]
playerColourList 3 = [Green, Purple, Orange]
playerColourList 4 = [Blue, Purple, Orange, Black]
playerColourList 6 = [Green, Blue, Purple, Red, Orange, Black]
playerColourList _ = []

--Basic Operators--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- determine the color of a piece
getColour :: BoardPos -> Maybe Colour
getColour (G _) = Just Green
getColour (B _) = Just Blue
getColour (P _) = Just Purple
getColour (R _) = Just Red
getColour (O _) = Just Orange
getColour (K _) = Just Black
getColour _ = Nothing -- type E and U do not contain any colour information
-- compare the colour of two board positions
compareColour :: BoardPos -> Colour -> Bool
compareColour bPos colour = case getColour bPos of
                                Just bcolour -> bcolour == colour
                                Nothing -> False
-- identify if a position on the board is empty
isEmpty :: BoardPos -> Bool
isEmpty (E _) = True
isEmpty _ = False
-- identify if a position on the board is occupied
isOccupied :: BoardPos -> Bool
isOccupied bPos = case getColour bPos of
                    Just _ -> True
                    _      -> False
-- type U should be ignored when rendering, therefore, skips as it is just a spacer                    
isSpacer :: BoardPos -> Bool
isSpacer (U _) = True
isSpacer _ = False

-- get the assoicated position
getPos :: BoardPos -> Pos
getPos (U p) = p
getPos (K p) = p
getPos (O p) = p
getPos (P p) = p
getPos (R p) = p
getPos (B p) = p
getPos (G p) = p
getPos (E p) = p

-- update the new colour state for a board position
-- can be combined when applying "getColour" function
safeRepaint :: Maybe Colour -> BoardPos -> BoardPos
safeRepaint Nothing b = b -- just do nothing if the given colour is nothing
safeRepaint (Just colour) b = repaint colour b
-- change the colour state and return the new board position
repaint :: Colour -> BoardPos -> BoardPos
repaint Green b  = G (getPos b)
repaint Black b  = K (getPos b)
repaint Orange b = O (getPos b)
repaint Red b    = R (getPos b)
repaint Blue b   = B (getPos b)
repaint Purple b = P (getPos b)
-- discard a position's colour state
erase :: BoardPos -> BoardPos
erase b = E (getPos b)

--Board Operators--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- the main board that contains the overall states for displaying and for movement handling
-- to improve the performance, the mutation will be handled under ST monad, and somtimes with parallelism
externalBoard :: Board
externalBoard = [
        [U(0, 0),  U(1, 0),  U(2, 0),  U(3, 0),  U(4, 0),  U(5, 0),  U(6, 0),  U(7, 0),  U(8, 0),  G(9, 0),  U(10, 0),  U(11, 0),  U(12, 0),  U(13, 0),  U(14, 0),  U(15, 0),  U(16, 0),  U(17, 0),  U(18, 0)],
        [U(0, 1),  U(1, 1),  U(2, 1),  U(3, 1),  U(4, 1),  U(5, 1),  U(6, 1),  U(7, 1),  G(8, 1),  U(9, 1),  G(10, 1),  U(11, 1),  U(12, 1),  U(13, 1),  U(14, 1),  U(15, 1),  U(16, 1),  U(17, 1),  U(18, 1)],
        [U(0, 2),  U(1, 2),  U(2, 2),  U(3, 2),  U(4, 2),  U(5, 2),  U(6, 2),  G(7, 2),  U(8, 2),  G(9, 2),  U(10, 2),  G(11, 2),  U(12, 2),  U(13, 2),  U(14, 2),  U(15, 2),  U(16, 2),  U(17, 2),  U(18, 2)],
        [B(0, 3),  U(1, 3),  B(2, 3),  U(3, 3),  B(4, 3),  U(5, 3),  E(6, 3),  U(7, 3),  E(8, 3),  U(9, 3),  E(10, 3),  U(11, 3),  E(12, 3),  U(13, 3),  K(14, 3),  U(15, 3),  K(16, 3),  U(17, 3),  K(18, 3)],
        [U(0, 4),  B(1, 4),  U(2, 4),  B(3, 4),  U(4, 4),  E(5, 4),  U(6, 4),  E(7, 4),  U(8, 4),  E(9, 4),  U(10, 4),  E(11, 4),  U(12, 4),  E(13, 4),  U(14, 4),  K(15, 4),  U(16, 4),  K(17, 4),  U(18, 4)],
        [U(0, 5),  U(1, 5),  B(2, 5),  U(3, 5),  E(4, 5),  U(5, 5),  E(6, 5),  U(7, 5),  E(8, 5),  U(9, 5),  E(10, 5),  U(11, 5),  E(12, 5),  U(13, 5),  E(14, 5),  U(15, 5),  K(16, 5),  U(17, 5),  U(18, 5)],
        [U(0, 6),  U(1, 6),  U(2, 6),  E(3, 6),  U(4, 6),  E(5, 6),  U(6, 6),  E(7, 6),  U(8, 6),  E(9, 6),  U(10, 6),  E(11, 6),  U(12, 6),  E(13, 6),  U(14, 6),  E(15, 6),  U(16, 6),  U(17, 6),  U(18, 6)],
        [U(0, 7),  U(1, 7),  P(2, 7),  U(3, 7),  E(4, 7),  U(5, 7),  E(6, 7),  U(7, 7),  E(8, 7),  U(9, 7),  E(10, 7),  U(11, 7),  E(12, 7),  U(13, 7),  E(14, 7),  U(15, 7),  O(16, 7),  U(17, 7),  U(18, 7)],
        [U(0, 8),  P(1, 8),  U(2, 8),  P(3, 8),  U(4, 8),  E(5, 8),  U(6, 8),  E(7, 8),  U(8, 8),  E(9, 8),  U(10, 8),  E(11, 8),  U(12, 8),  E(13, 8),  U(14, 8),  O(15, 8),  U(16, 8),  O(17, 8),  U(18, 8)],
        [P(0, 9),  U(1, 9),  P(2, 9),  U(3, 9),  P(4, 9),  U(5, 9),  E(6, 9),  U(7, 9),  E(8, 9),  U(9, 9),  E(10, 9),  U(11, 9),  E(12, 9),  U(13, 9),  O(14, 9),  U(15, 9),  O(16, 9),  U(17, 9),  O(18, 9)],
        [U(0, 10), U(1, 10), U(2, 10), U(3, 10), U(4, 10), U(5, 10), U(6, 10), R(7, 10), U(8, 10), R(9, 10), U(10, 10), R(11, 10), U(12, 10), U(13, 10), U(14, 10), U(15, 10), U(16, 10), U(17, 10), U(18, 10)],
        [U(0, 11), U(1, 11), U(2, 11), U(3, 11), U(4, 11), U(5, 11), U(6, 11), U(7, 11), R(8, 11), U(9, 11), R(10, 11), U(11, 11), U(12, 11), U(13, 11), U(14, 11), U(15, 11), U(16, 11), U(17, 11), U(18, 11)],
        [U(0, 12), U(1, 12), U(2, 12), U(3, 12), U(4, 12), U(5, 12), U(6, 12), U(7, 12), U(8, 12), R(9, 12), U(10, 12), U(11, 12), U(12, 12), U(13, 12), U(14, 12), U(15, 12), U(16, 12), U(17, 12), U(18, 12)]
    ]

-- access an element in a 2D list 
-- wrap the list as the state
getElement :: Pos -> State [[a]] a
getElement (x, y) = do n <- get
                       return ((n !! y) !! x)
-- mutate an element in a 2d list
replace2 :: Pos -> a -> [[a]] -> [[a]]
replace2 (x, y) new ls = let row = replace x new (ls !! y)
                         in  replace y row ls
-- mutate an element in a list
replace :: Int -> a -> [a] -> [a]
replace idx new ls = front ++ [new] ++ end
    where
        (front, _:end) = splitAt idx ls
-- find the all pieces' positions
findAllPieces :: Board -> [BoardPos]
findAllPieces = concatMap (filter isOccupied)
-- find the all pieces' positions on the board based on the colour
findPiecesWithColour :: Colour -> Board -> [BoardPos]
findPiecesWithColour colour = concatMap (filter (`compareColour` colour))
-- mutate a position of the whole board with certain function
changeBoardElement :: (BoardPos -> BoardPos) -> BoardPos -> Board -> Board
changeBoardElement f bPos board = let new = f bPos
                                      pos = getPos bPos
                                  in  new `par` pos `pseq` replace2 pos new board
-- erase the pieces on the board and keep only certain coloured pieces, 
-- this is to generate different board according to the input players amount
eraseBoard :: [Colour] -> Board
eraseBoard colourList = runST $ do n <- newSTRef externalBoard
                                   modifySTRef n (map (eraseRow colourList))
                                   readSTRef n
    where
        eraseRow :: [Colour] -> [BoardPos] -> [BoardPos]
        eraseRow _ [] = []
        eraseRow cs (x:xs) = case getColour x of
                                Nothing -> x:eraseRow cs xs
                                Just c  -> if c `notElem` cs then erase x:eraseRow cs xs
                                           else x:eraseRow cs xs
-- given two positions, modify there colours to build a route/path
repaintPath :: Board -> BoardPos -> BoardPos -> Board
repaintPath board start end = let colour = getColour start
                              in  runST $ do n <- newSTRef board
                                             modifySTRef n (changeBoardElement erase start) -- erase the starting position's colour
                                             modifySTRef n (changeBoardElement (safeRepaint colour) end) -- over-write the ending position's colour
                                             readSTRef n

--Movement Operators-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Game Rules: players move their pieces one after another based on turn, 
-- the first player that manages moving all of his pieces to the oppsite corner of the start wins the game
-- their are two movements allowed in the game: one is to simply jump to the adjacent empty position
-- another is to jump over one occupied position to another empty position, called chained jump, becuase this type of jump could be made multiple time for a larger distance
-- at every turn only one of the jumps is allowed, that is, once a movement is made, the player cannot make another
-- the bases other than the start and goal are not allowed for a player to stay long: either restricting the move to other base
-- or allowing piece pass other bases but not allowed to settle
-- additionally, once a piece successfully enter the goal base, it cannot move out, but is still moveable within the base


-- enter a board position and return a list of avaliable movements/reachable positions: adjacent jump and chained jump
destinationList :: BoardPos -> State Board [BoardPos]
destinationList bPos = do case getColour bPos of
                            Nothing -> return [] -- invalid chosen pieces
                            Just c  -> do adjacentMoves <- findAvaliableNeighbors c bPos -- find the destinations of two jumps
                                          chainedMoves  <- recursiveSearch [] c bPos -- start with empty record
                                          let combinedList = adjacentMoves `par` chainedMoves `pseq` nub (adjacentMoves ++ chainedMoves) -- combine two positions list and discard the repeated ones
                                          return combinedList

-- detect the adjacent positions that are valid to jump to 
findAvaliableNeighbors :: Colour -> BoardPos -> State Board [BoardPos]
findAvaliableNeighbors c bPos = do neighbourList <- mapM getElement (neighborPosList c (getPos bPos)) -- return the adjacent positions surrounding the entered position
                                   return (filter isEmpty neighbourList) -- discard the positions that are occupied
    where
        neighborPosList :: Colour -> Pos -> [Pos]
        neighborPosList c p@(x, y) = filter (validMoveCheck c p) [(x-1, y-1), (x-2, y), (x-1, y+1), (x+1, y+1), (x+2, y), (x+1, y-1)]

        validMoveCheck :: Colour -> Pos -> Pos -> Bool
        validMoveCheck c f t = testValidPos boardWidth boardHeight t && testCorners c t && baseMoveAllow c f t

-- chained jump : a list of one-over hop chained together to reach a larger jump range
-- recursively search for the reachable destinations for chained jumps of different directions
-- while search, a list of discovered positions is maintained to avoid cycling/repetition 
recursiveSearch :: [BoardPos] -> Colour -> BoardPos -> State Board [BoardPos]
recursiveSearch record c bPos = do chainJumpList <- jumpDirection c bPos
                                   let renewList = filter (`notElem` record) chainJumpList -- discard the positions already found
                                       newRecord = renewList ++ record -- adding the new discovered positions to the recorded positions
                                   recursiveList <- concatMapM (recursiveSearch newRecord c) renewList -- continue the next "layer" of search based on the positions discovered at this "layer"
                                   -- until not new positions are found
                                   return (recursiveList ++ renewList) -- combining all found positions and return together

-- given a board position, check the validty of all directions of one over hop
jumpDirection :: Colour -> BoardPos -> State Board [BoardPos]
jumpDirection c bpos = do reachableList <- mapM (determineValidJump c bpos) [f (-1, -1), f (-2, 0), f (-1, 1), f (1, 1), f (2, 0), f (1, -1)] -- return the searched destinations
                          return $ filter (/= bpos) reachableList -- remove the invalid moves

    where
        f (a, b) (x, y) = (a+x, b+y)

-- check the whether the one over hop from a position of a certain direction is valid, if it is then return the destination, otherwise, return itself
determineValidJump :: Colour -> BoardPos -> (Pos -> Pos) -> State Board BoardPos
determineValidJump c bpos f = if not $ validMoveCheck c then return bpos -- if invalid then just return the initial position 
                              else do n1 <- getElement fp
                                      n2 <- getElement fp2
                                      if n1 `par` n2 `pseq` isOccupied n1 && isEmpty n2 then return n2 -- check if the pre-condition of the chained jump is satisfied
                                      else return bpos -- if not then means it is not valid as well                                                   
    where
        pos = getPos bpos
        fp = f pos
        fp2 = (f . f) pos
        -- a valid movement means both positions should within the board
        -- and should not stay at other's bases, as well as the satisfying the restriction of goal base
        validMoveCheck :: Colour -> Bool
        validMoveCheck c = testValidPos boardWidth boardHeight fp2 && testCorners c fp2 && baseMoveAllow c pos fp2

-- test if a piece's position is out of boarder
testValidPos :: Int -> Int -> Pos -> Bool
testValidPos xlimt ylimt (x, y) = x >= 0 && y >= 0 && x < xlimt && y < ylimt

-- an addition check will be first projecting the board into a square occupied board for certain player and test whether it exists in that board
-- this is to ensure that the piece will only pass by other's bases but not stay long
testCorners :: Colour -> Pos -> Bool
testCorners colour pos = testValidPos occupiedBoardSize occupiedBoardSize (projection colour pos)

-- besides, once a piece enter the goal base, it's not allowed to move out again, but could still move around within the base
baseMoveAllow :: Colour -> Pos -> Pos -> Bool
baseMoveAllow colour f t = let fp = projection colour f
                               tp = projection colour t
                           in  (fp `notElem` goalBase) || (tp `elem` goalBase)

--Projection Operator----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- the positions of the ending and starting states
goalBase :: [Pos]
goalBase = [(0,4),(0,5),(1,5),(0,6),(1,6),(2,6)]
startBase :: [Pos]
startBase = [(4,0),(5,0),(6,0),(5,1),(6,1),(6,2)]

-- the projection of the main (display) board to the sub-occupied board for each player
projection :: Colour -> Pos -> Pos
projection Green   = projectGreen
projection Blue    = projectBlue
projection Purple  = projectPurple
projection Red     = projectRed
projection Orange  = projectOrange
projection Black   = projectBlack
-- the projection of the sub-occupied board of certain player to the main (display) board 
reversion :: Colour -> Pos -> Pos
reversion Green    = reverseGreen
reversion Blue     = reverseBlue
reversion Purple   = reversePurple
reversion Red      = reverseRed
reversion Orange   = reverseOrange
reversion Black    = reverseBlack
-- Below is the coordinate conversion of internal board state of each colour and the external display board state
-- through modifying the x-y coordinates 
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


--Board Display----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- print the board (main and sub) in terminal: debugging usage

-- occupied board printing
printBoard :: OccupiedBoard -> IO ()
printBoard b = do putStrLn ""
                  printBoard' b
    where
        printBoard' [] = putStrLn ""
        printBoard' (x:xs) = do print x -- putStrLn (convertWithTabs x)
                                printBoard' xs
        convertWithTabs [] = ""
        convertWithTabs (x:xs) = show x ++ "\t" ++ convertWithTabs xs

-- main board printing
printEoard :: Board -> IO ()
printEoard b = do putStrLn ""
                  printEoard' (testDisplay b)
    where
        printEoard' :: [[Int]] -> IO ()
        printEoard' [] = putStrLn ""
        printEoard' (x:xs) = do let str = map skipZero (show x)
                                putStrLn str
                                printEoard' xs

        skipZero :: Char -> Char
        skipZero '0' = ' '
        skipZero ',' = ' '
        -- skipZero '9' = '0'
        skipZero a = a

-- transform the board positions into numerical values
testDisplay :: Board -> [[Int]]
testDisplay = map testDisplay'
    where
        testDisplay' :: [BoardPos] -> [Int]
        testDisplay' [] = []
        testDisplay' (x:xs) = case getColour x of
                                Just c -> colourToIndex c + 1:testDisplay' xs
                                _ -> 0:testDisplay' xs
                                {-Just Green -> 1:testDisplay' xs
                                Just Red -> 9:testDisplay' xs
                                Just _ -> 0:testDisplay' xs
                                Nothing -> case isEmpty x of 
                                            True -> 9:testDisplay' xs
                                            False -> 0:testDisplay' xs-} 
                                
        colourToIndex :: Colour -> Int
        colourToIndex colour = fromMaybe 0 (elemIndex colour (playerColourList 6))