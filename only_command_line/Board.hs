module Board where
-- The operator of displaying board state
import Data.Maybe ( fromMaybe, isJust )
import Data.List ( elemIndex, nub, intersperse, intercalate )
import Control.Monad.ST ( runST )
import Data.STRef ( modifySTRef, newSTRef, readSTRef )
import Control.Monad.State ( State, MonadState(get), evalState )
import Control.Monad.Extra ( concatMapM )
import Control.Parallel ( par, pseq )
import qualified Data.HashMap.Strict as HashMap

-- main :: IO ()
-- main = do
--     -- 創建一個空的哈希表
--     let emptyHashMap = HashMap.empty

--     -- 添加鍵值對到哈希表
--     let hashMap = HashMap.insert "one" 1 emptyHashMap
--         updatedHashMap = HashMap.insert "two" 2 hashMap

--     -- 查找特定鍵對應的值
--     case HashMap.lookup "two" updatedHashMap of
--         Just value -> putStrLn $ "The value for key 'two' is: " ++ show value
--         Nothing -> putStrLn "Key 'two' not found in the hash map"


-- there are six colours and two additional board statuses being applied in the game
-- Green, Blue, Purple, Red, Orange, Black, Empty and Unknown
data Status = G | B | P | R | O | K | E | U deriving (Eq, Show)
-- a position is consisted of x and y coordinates
type Pos = (Int, Int)
-- the board position should be able to represent the occupy state and the piece's colour
type BoardPos = (Pos, Status)
-- a hashmap is used to hold all board statuses
type Board = HashMap.HashMap Pos Status
-- a movement is the transform of a piece from one position to another: (beginning, destination) and the piece's colour
type Transform = ((Pos, Pos), Status)

-- the board size: (width, height)
boardSize :: (Int, Int)
boardSize = (19, 13)

-- the colour corresponding to each player with different number of players allowed
playerColourList :: Int -> [Status]
playerColourList 2 = [G, R]
playerColourList 3 = [G, P, O]
playerColourList 4 = [B, P, O, K]
playerColourList 6 = [G, B, P, R, O, K]
playerColourList _ = error "Invalid number of players"
-- given a colour and the total number of players, return the corresponding player's index
colourIndex :: Status -> Int -> Maybe Int
colourIndex colour pn = colour `elemIndex` playerColourList pn

--Basic Operators--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- determine a position's status 
getStatus :: BoardPos -> Status
getStatus (_, s) = s
-- extract the associated coordinate
getPos :: BoardPos -> Pos
getPos (p, _) = p
-- identify if a position is free
isEmpty :: BoardPos -> Bool
isEmpty (_, E) = True
isEmpty _ = False
-- determine if a position (U) is used as a spacer when rendering                    
isSpacer :: BoardPos -> Bool
isSpacer (_, U) = True
isSpacer _ = False
-- identify if a position is occupied by a piece
isOccupied :: BoardPos -> Bool
isOccupied x = not (isEmpty x) && not (isSpacer x)

-- change the status of a position
repaint :: Status -> BoardPos -> BoardPos
repaint s (p, _) = (p, s)
-- reset a position's status
erase :: BoardPos -> BoardPos
erase (p, _) = (p, E)

--Board Operators--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- the main board that contains the overall states for displaying and for movement handling
boardList :: [BoardPos]
boardList = [
    ((0,0),U),((1,0),U),((2,0),U),((3,0),U),((4,0),U),((5,0),U),((6,0),U),((7,0),U),((8,0),U),((9,0),G),((10,0),U),((11,0),U),((12,0),U),((13,0),U),((14,0),U),((15,0),U),((16,0),U),((17,0),U),((18,0),U),
    ((0,1),U),((1,1),U),((2,1),U),((3,1),U),((4,1),U),((5,1),U),((6,1),U),((7,1),U),((8,1),G),((9,1),U),((10,1),G),((11,1),U),((12,1),U),((13,1),U),((14,1),U),((15,1),U),((16,1),U),((17,1),U),((18,1),U),
    ((0,2),U),((1,2),U),((2,2),U),((3,2),U),((4,2),U),((5,2),U),((6,2),U),((7,2),G),((8,2),U),((9,2),G),((10,2),U),((11,2),G),((12,2),U),((13,2),U),((14,2),U),((15,2),U),((16,2),U),((17,2),U),((18,2),U),
    ((0,3),B),((1,3),U),((2,3),B),((3,3),U),((4,3),B),((5,3),U),((6,3),E),((7,3),U),((8,3),E),((9,3),U),((10,3),E),((11,3),U),((12,3),E),((13,3),U),((14,3),K),((15,3),U),((16,3),K),((17,3),U),((18,3),K),
    ((0,4),U),((1,4),B),((2,4),U),((3,4),B),((4,4),U),((5,4),E),((6,4),U),((7,4),E),((8,4),U),((9,4),E),((10,4),U),((11,4),E),((12,4),U),((13,4),E),((14,4),U),((15,4),K),((16,4),U),((17,4),K),((18,4),U),
    ((0,5),U),((1,5),U),((2,5),B),((3,5),U),((4,5),E),((5,5),U),((6,5),E),((7,5),U),((8,5),E),((9,5),U),((10,5),E),((11,5),U),((12,5),E),((13,5),U),((14,5),E),((15,5),U),((16,5),K),((17,5),U),((18,5),U),
    ((0,6),U),((1,6),U),((2,6),U),((3,6),E),((4,6),U),((5,6),E),((6,6),U),((7,6),E),((8,6),U),((9,6),E),((10,6),U),((11,6),E),((12,6),U),((13,6),E),((14,6),U),((15,6),E),((16,6),U),((17,6),U),((18,6),U),
    ((0,7),U),((1,7),U),((2,7),P),((3,7),U),((4,7),E),((5,7),U),((6,7),E),((7,7),U),((8,7),E),((9,7),U),((10,7),E),((11,7),U),((12,7),E),((13,7),U),((14,7),E),((15,7),U),((16,7),O),((17,7),U),((18,7),U),
    ((0,8),U),((1,8),P),((2,8),U),((3,8),P),((4,8),U),((5,8),E),((6,8),U),((7,8),E),((8,8),U),((9,8),E),((10,8),U),((11,8),E),((12,8),U),((13,8),E),((14,8),U),((15,8),O),((16,8),U),((17,8),O),((18,8),U),
    ((0,9),P),((1,9),U),((2,9),P),((3,9),U),((4,9),P),((5,9),U),((6,9),E),((7,9),U),((8,9),E),((9,9),U),((10,9),E),((11,9),U),((12,9),E),((13,9),U),((14,9),O),((15,9),U),((16,9),O),((17,9),U),((18,9),O),
    ((0,10),U),((1,10),U),((2,10),U),((3,10),U),((4,10),U),((5,10),U),((6,10),U),((7,10),R),((8,10),U),((9,10),R),((10,10),U),((11,10),R),((12,10),U),((13,10),U),((14,10),U),((15,10),U),((16,10),U),((17,10),U),((18,10),U),
    ((0,11),U),((1,11),U),((2,11),U),((3,11),U),((4,11),U),((5,11),U),((6,11),U),((7,11),U),((8,11),R),((9,11),U),((10,11),R),((11,11),U),((12,11),U),((13,11),U),((14,11),U),((15,11),U),((16,11),U),((17,11),U),((18,11),U),
    ((0,12),U),((1,12),U),((2,12),U),((3,12),U),((4,12),U),((5,12),U),((6,12),U),((7,12),U),((8,12),U),((9,12),R),((10,12),U),((11,12),U),((12,12),U),((13,12),U),((14,12),U),((15,12),U),((16,12),U),((17,12),U),((18,12),U)
    ]

-- print the board in terminal, for debugging
printEoard :: [BoardPos] -> IO ()
printEoard b = do putStrLn ""
                  printEoard' b
    where
        printEoard' [] = putStrLn ""
        printEoard' xs = do let str = map toChar (take count xs)
                            putStrLn $ intersperse ' ' str
                            printEoard' (drop count xs)

        count = fst boardSize

        toChar (_, G) = 'G'
        toChar (_, B) = 'B'
        toChar (_, P) = 'P'
        toChar (_, R) = 'R'
        toChar (_, O) = 'O'
        toChar (_, K) = 'K'
        toChar (_, E) = 'E'
        toChar (_, U) = ' '

-- rearrange the board as a hashmap 
boardMap :: Board
boardMap = HashMap.fromList boardList

-- mutate an element in a 2D list
-- replace2 :: Pos -> Board -> [[a]] -> [[a]]
-- replace2 (x, y) newItem table = let newRow = replace x newItem (table !! y)
--                                 in  replace y newRow table

-- mutate an element in a list
-- replace :: Int -> a -> [a] -> [a]
-- replace idx newItem row = front ++ [newItem] ++ end
--     where
--         (front, _:end) = splitAt idx row

-- remove an element in a list
-- removeByIdx :: Int -> [a] -> [a]
-- removeByIdx idx row = front ++ end
--     where
--         (front, _:end) = splitAt idx row

-- find the all pieces' positions on the board based on the colour
-- could be used when needed to generate an occupied board for certain player
-- findPiecesWithColour :: Colour -> Board -> [BoardPos]
-- findPiecesWithColour colour = concatMap (filter (`compareColour` colour))

-- mutate a position of the board with a certain function, either "repaint" or "erase"
changeBoardElement :: (BoardPos -> BoardPos) -> BoardPos -> Board -> Board
changeBoardElement f bPos board = let (pos, s) = f bPos
                                  in  HashMap.insert pos s board
{-
-- erase the pieces on the board and keep only certain coloured pieces, 
-- this is to generate different board according to the different numbers of players
eraseBoard :: [Colour] -> Board -> Board
eraseBoard keptColours eboard = runST $ do n <- newSTRef eboard
                                           modifySTRef n (map (eraseRow keptColours))
                                           readSTRef n
    where
        eraseRow :: [Colour] -> [BoardPos] -> [BoardPos]
        eraseRow _ [] = []
        eraseRow cs (x:xs) = case getColour x of
                                Nothing -> x:eraseRow cs xs
                                Just c  -> if c `notElem` cs then erase x:eraseRow cs xs else x:eraseRow cs xs

-- given two positions, modify their colour states to form a movement
repaintPath :: Board -> Transform -> Board
repaintPath eboard (start, end) = case getColour start of
                                    Nothing -> error "Invalid start position for a movement"
                                    Just colour -> runST $
                                                    do n <- newSTRef eboard
                                                       modifySTRef n (changeBoardElement erase start) -- erase the start position's colour
                                                       modifySTRef n (changeBoardElement (repaint colour) end) -- over-write the end position's colour
                                                       readSTRef n
--Movement Operators-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Game Rules: players move their pieces one after another based on turn, 
-- the first player that manages moving all of his pieces to the opposite corner of the start base wins the game
-- there are two movements allowed in the game: one is to simply jump to the adjacent empty position (step)
-- another is to jump over one occupied position to another empty position, called chained jump (or hops), 
-- this type of jump could be made multiple time for a larger distance, and at every turn only one of the jumps is allowed
-- besides, the bases other than the start and goal are not allowed for a player to stay
-- additionally, once a piece successfully enter the goal base, it cannot move out, but is still moveable within the base

-- enter a board position and return a list of available movements/reachable positions: adjacent jump and chained jump
destinationList :: BoardPos -> State Board [BoardPos]
destinationList bPos = do case getColour bPos of
                            Nothing -> return [] -- invalid chosen pieces
                            Just c  -> do -- find the reachable destinations for steps
                                          adjacentMoves <- findAvaliableNeighbors c bPos
                                          -- find the reachable destinations for hops
                                          chainedMoves  <- recursiveSearch [] c bPos
                                          -- combine the two lists and discard the repeated ones
                                          return $ adjacentMoves `par` chainedMoves `pseq` nub (adjacentMoves ++ chainedMoves)

-- detect the adjacent positions that are available to move to 
findAvaliableNeighbors :: Colour -> BoardPos -> State Board [BoardPos]
findAvaliableNeighbors c bPos = do -- generate the adjacent positions surrounding the entered position
                                   -- and retrieve them from the board
                                   neighbourList <- mapM getElement (neighborPosList c (getPos bPos))
                                   -- discard the positions that are already occupied
                                   return (filter isEmpty neighbourList)
    where
        -- generate positions that is around a given position, and test the validity
        neighborPosList :: Colour -> Pos -> [Pos]
        neighborPosList c p@(x, y) = filter (validMoveCheck c p) [(x-1, y-1), (x-2, y), (x-1, y+1), (x+1, y+1), (x+2, y), (x+1, y-1)]

        -- the generated neighbour's positions should be within the board size, as well as the fitting the occupied board once being projected
        -- also, need to follow the restriction of moving within the goal base
        validMoveCheck :: Colour -> Pos -> Pos -> Bool
        validMoveCheck c f t = let validPosition = testValidPos boardWidth boardHeight t
                                   notAccessCorner = testCorners c t
                                   withinBase = baseMoveAllow c f t
                               in  validPosition `par` notAccessCorner `par` withinBase `pseq` (validPosition && notAccessCorner && withinBase)

-- generate a list of one-over jump chained together to reach a larger jump range
-- recursively search for the reachable destinations for chained jumps of different directions
-- during the search, a list of discovered positions is maintained to avoid cycling/repetition, 
-- while another list stores the new frontiers based on the previous list of positions
recursiveSearch :: [BoardPos] -> Colour -> BoardPos -> State Board [BoardPos]
recursiveSearch record c bPos = do -- generate a new list of positions found
                                   chainJumpList <- jumpDirection c bPos
                                   -- discard the positions that are already found
                                   let renewList = filter (`notElem` record) chainJumpList
                                       -- adding the new discovered positions to the record
                                       newRecord = renewList ++ record
                                   -- continue the next "layer" of search based on the positions discovered at this "layer"
                                   recursiveList <- concatMapM (recursiveSearch newRecord c) renewList
                                   -- until not new positions are found, return the combined list of all found positions
                                   -- might exist duplicated positions, but will be omitted at the final integration
                                   return (recursiveList ++ renewList)

-- given a board position, check the validity of all directions of hopping
jumpDirection :: Colour -> BoardPos -> State Board [BoardPos]
jumpDirection c bpos = do -- return the reachable destinations
                          reachableList <- mapM (determineValidJump c bpos) [f (-1, -1), f (-2, 0), f (-1, 1), f (1, 1), f (2, 0), f (1, -1)]
                          return $ filter (/= bpos) reachableList -- and remove the invalid moves
    where
        f (a, b) (x, y) = (a+x, b+y)

-- check the whether the hop from a certain direction is valid, if it is then return the destination, otherwise, return itself (will then be eliminated)
determineValidJump :: Colour -> BoardPos -> (Pos -> Pos) -> State Board BoardPos
determineValidJump c bpos f = if not (validMoveCheck c) then return bpos -- if invalid then just return the initial position 
                              else do n1 <- getElement fp
                                      n2 <- getElement fp2
                                      -- check if the pre-condition of the chained jump is satisfied
                                      if n1 `par` n2 `pseq` (isOccupied n1 && isEmpty n2) then return n2
                                      else return bpos -- if not then means it is not valid as well                                                   
    where
        pos = getPos bpos
        -- the adjacent position
        fp = f pos
        -- the position that is separated two positions away
        fp2 = (f . f) pos

        -- a valid movement means both positions should within the board
        -- and should not stay at other's bases, as well as the satisfying the restriction of goal base
        validMoveCheck :: Colour -> Bool
        validMoveCheck c = testValidPos boardWidth boardHeight fp2 && testCorners c fp2 && baseMoveAllow c pos fp2

-- test if a piece's position is out of border
testValidPos :: Int -> Int -> Pos -> Bool
testValidPos xlimt ylimt (x, y) = x >= 0 && y >= 0 && x < xlimt && y < ylimt

-- an addition check, first project the board into a square occupied board for certain player and test whether it exists in that board
-- this is to ensure that the piece will not pass by other's bases
testCorners :: Colour -> Pos -> Bool
testCorners colour pos = testValidPos occupiedBoardSize occupiedBoardSize (projection colour pos)

-- besides, once a piece enter the goal base, it's not allowed to move out again, but could still move around within the base
baseMoveAllow :: Colour -> Pos -> Pos -> Bool
baseMoveAllow colour start end = let sp = projection colour start
                                     ep = projection colour end
                                 in  sp `par` ep `pseq` (sp `notElem` goalBase) || (ep `elem` goalBase)

--Projection Operator----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- the projected positions of the end and start based within the occupied board
goalBase :: [Pos]
goalBase = [(0,4),(0,5),(1,5),(0,6),(1,6),(2,6)]
startBase :: [Pos]
startBase = [(4,0),(5,0),(6,0),(5,1),(6,1),(6,2)]

-- project the movement to the internal (occupied) board based on certain colour
projectMove :: Colour -> Transform -> (Pos, Pos)
projectMove colour (x, y) = let px = projection colour (getPos x)
                                py = projection colour (getPos y)
                            in  px `par` py `pseq` (px, py)

-- the projection of the main board to the occupied board for each player (colour)
projection :: Colour -> Pos -> Pos
projection Green   = projectGreen
projection Blue    = projectBlue
projection Purple  = projectPurple
projection Red     = projectRed
projection Orange  = projectOrange
projection Black   = projectBlack

-- revert the projected position to the main board ones
reversion :: Colour -> Pos -> Pos
reversion Green    = reverseGreen
reversion Blue     = reverseBlue
reversion Purple   = reversePurple
reversion Red      = reverseRed
reversion Orange   = reverseOrange
reversion Black    = reverseBlack

-- Below is the coordinate conversion of internal board state of each colour and the external display board state
-- mainly mathematical computation, can be ignored

-- through modifying the x-y coordinates 
{-
        (1,1)
        (6,9) (7,8) (8,7) (9,6)
        (5,8) (6,7) (7,6) (8,5)
        (4,7) (5,6) (6,5) (7,4)
(1,-1)  (3,6) (4,5) (5,4) (6,3)
-}projectGreen :: Pos -> Pos
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
-}


