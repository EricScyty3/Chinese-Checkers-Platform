module NBoard where
-- The operator of displaying board state
import Data.Maybe ( fromMaybe, isJust )
import Data.List ( elemIndex, nub, intersperse, intercalate, sort, sortBy )
import Control.Monad.ST ( runST )
import Data.STRef ( modifySTRef, newSTRef, readSTRef )
import Control.Monad.State ( State, MonadState(get), evalState )
import Control.Monad.Extra ( concatMapM )
import Control.Parallel ( par, pseq )
import qualified Data.HashMap.Strict as HM
import Control.Parallel.Strategies (parMap, rseq)
import qualified Data.Set as Set
import NProjection
    ( projectBlack,
      projectBlue,
      projectGreen,
      projectOrange,
      projectPurple,
      projectRed,
      reverseBlack,
      reverseBlue,
      reverseGreen,
      reverseOrange,
      reversePurple,
      reverseRed, goalBase )

-- remove the duplicate elements in a list based on the Set data structure
-- continues checking elements in a built Set
setNub :: (Ord a) => [a] -> [a]
setNub = fill Set.empty
  where
    fill _ [] = []
    fill s (x:xs) = if x `Set.member` s then fill s xs
                    else x : fill (Set.insert x s) xs

-- there are six colours and two additional board statuses being applied in the game
-- Green, Blue, Purple, Red, Orange, Black, Empty and Unknown
data Status = G | B | P | R | O | K | E | U deriving (Eq, Show, Ord)
-- a position is consisted of x and y coordinates
type Pos = (Int, Int)
-- the board position should be able to represent the occupy state and the piece's colour
type BoardPos = (Pos, Status)
-- a hashmap is used to hold all board statuses
type Board = HM.HashMap Pos Status
-- a movement is the transform of a piece from one position to another: (beginning, destination) and the piece's colour
type Transform = ((Pos, Pos), Status)

-- the board size: (height, width)
boardSize :: (Int, Int)
boardSize = (13, 19)

occupiedBoardSize :: Int
occupiedBoardSize = 7

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

-- a state used to represent the error state such as Nothing or positions need to be discarded
-- errorState :: BoardPos
-- errorState = ((-1, -1), U)
-- -- determine if a state is not accepted
-- isError :: BoardPos -> Bool
-- isError bs = bs == errorState

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
repaint _ (p, U) = (p, U)
repaint s (p, _) = (p, s)
-- reset a position's status
erase :: BoardPos -> BoardPos
erase (p, U) = (p, U)
erase (p, _) = (p, E)

-- see if a piece's colour is the same as a given colour
comparePiece :: BoardPos -> Status -> Bool
comparePiece (_, x) s = x == s

--Board Operators--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- the main board that contains the overall states for displaying and for movement handling
boardList :: [BoardPos]
boardList = [
    ((0,0),U), ((0,1),U), ((0,2),U), ((0,3),U), ((0,4),U), ((0,5),U), ((0,6),U), ((0,7),U), ((0,8),U), ((0,9),G), ((0,10),U), ((0,11),U), ((0,12),U), ((0,13),U), ((0,14),U), ((0,15),U), ((0,16),U), ((0,17),U), ((0,18),U),
    ((1,0),U), ((1,1),U), ((1,2),U), ((1,3),U), ((1,4),U), ((1,5),U), ((1,6),U), ((1,7),U), ((1,8),G), ((1,9),U), ((1,10),G), ((1,11),U), ((1,12),U), ((1,13),U), ((1,14),U), ((1,15),U), ((1,16),U), ((1,17),U), ((1,18),U),
    ((2,0),U), ((2,1),U), ((2,2),U), ((2,3),U), ((2,4),U), ((2,5),U), ((2,6),U), ((2,7),G), ((2,8),U), ((2,9),G), ((2,10),U), ((2,11),G), ((2,12),U), ((2,13),U), ((2,14),U), ((2,15),U), ((2,16),U), ((2,17),U), ((2,18),U),
    ((3,0),B), ((3,1),U), ((3,2),B), ((3,3),U), ((3,4),B), ((3,5),U), ((3,6),E), ((3,7),U), ((3,8),E), ((3,9),U), ((3,10),E), ((3,11),U), ((3,12),E), ((3,13),U), ((3,14),K), ((3,15),U), ((3,16),K), ((3,17),U), ((3,18),K),
    ((4,0),U), ((4,1),B), ((4,2),U), ((4,3),B), ((4,4),U), ((4,5),E), ((4,6),U), ((4,7),E), ((4,8),U), ((4,9),E), ((4,10),U), ((4,11),E), ((4,12),U), ((4,13),E), ((4,14),U), ((4,15),K), ((4,16),U), ((4,17),K), ((4,18),U),
    ((5,0),U), ((5,1),U), ((5,2),B), ((5,3),U), ((5,4),E), ((5,5),U), ((5,6),E), ((5,7),U), ((5,8),E), ((5,9),U), ((5,10),E), ((5,11),U), ((5,12),E), ((5,13),U), ((5,14),E), ((5,15),U), ((5,16),K), ((5,17),U), ((5,18),U),
    ((6,0),U), ((6,1),U), ((6,2),U), ((6,3),E), ((6,4),U), ((6,5),E), ((6,6),U), ((6,7),E), ((6,8),U), ((6,9),E), ((6,10),U), ((6,11),E), ((6,12),U), ((6,13),E), ((6,14),U), ((6,15),E), ((6,16),U), ((6,17),U), ((6,18),U),
    ((7,0),U), ((7,1),U), ((7,2),P), ((7,3),U), ((7,4),E), ((7,5),U), ((7,6),E), ((7,7),U), ((7,8),E), ((7,9),U), ((7,10),E), ((7,11),U), ((7,12),E), ((7,13),U), ((7,14),E), ((7,15),U), ((7,16),O), ((7,17),U), ((7,18),U),
    ((8,0),U), ((8,1),P), ((8,2),U), ((8,3),P), ((8,4),U), ((8,5),E), ((8,6),U), ((8,7),E), ((8,8),U), ((8,9),E), ((8,10),U), ((8,11),E), ((8,12),U), ((8,13),E), ((8,14),U), ((8,15),O), ((8,16),U), ((8,17),O), ((8,18),U),
    ((9,0),P), ((9,1),U), ((9,2),P), ((9,3),U), ((9,4),P), ((9,5),U), ((9,6),E), ((9,7),U), ((9,8),E), ((9,9),U), ((9,10),E), ((9,11),U), ((9,12),E), ((9,13),U), ((9,14),O), ((9,15),U), ((9,16),O), ((9,17),U), ((9,18),O),
    ((10,0),U),((10,1),U),((10,2),U),((10,3),U),((10,4),U),((10,5),U),((10,6),U),((10,7),R),((10,8),U),((10,9),R),((10,10),U),((10,11),R),((10,12),U),((10,13),U),((10,14),U),((10,15),U),((10,16),U),((10,17),U),((10,18),U),
    ((11,0),U),((11,1),U),((11,2),U),((11,3),U),((11,4),U),((11,5),U),((11,6),U),((11,7),U),((11,8),R),((11,9),U),((11,10),R),((11,11),U),((11,12),U),((11,13),U),((11,14),U),((11,15),U),((11,16),U),((11,17),U),((11,18),U),
    ((12,0),U),((12,1),U),((12,2),U),((12,3),U),((12,4),U),((12,5),U),((12,6),U),((12,7),U),((12,8),U),((12,9),R),((12,10),U),((12,11),U),((12,12),U),((12,13),U),((12,14),U),((12,15),U),((12,16),U),((12,17),U),((12,18),U)
    ]

-- print the board in terminal, for debugging
printBoard :: Board -> IO()
printBoard b = let ls = HM.toList b
               in  printEoard $ sortBy (\(a, _) (b, _) -> compare a b) ls

printEoard :: [BoardPos] -> IO ()
printEoard b = do putStrLn ""
                  printEoard' b
    where
        printEoard' [] = putStrLn ""
        printEoard' xs = do let str = map toChar (take count xs)
                            putStrLn str
                            printEoard' (drop count xs)

        count = snd boardSize

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
boardMap = HM.fromList boardList

updateBoard :: BoardPos -> Board -> Board
updateBoard (pos, s) = HM.insert pos s

updating :: [BoardPos] -> Board -> Board
updating xs b = foldr updateBoard b xs

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
findPieces :: Status -> Board -> [BoardPos]
findPieces s board = HM.toList $ HM.filter (== s) board

-- mutate a position of the board with a certain function
-- changeBoardValue :: (BoardPos -> BoardPos) -> BoardPos -> Board -> Board
-- changeBoardValue f bPos = updateBoard (f bPos)

-- erase the pieces on the board and keep only certain coloured pieces, 
-- this is to generate different board according to the different numbers of players
eraseBoard :: [Status] -> Board -> Board
eraseBoard cs = HM.map (\x -> if x `elem` cs then E else x)

-- given two positions, modify their states to form a movement: erase the start and paint the end
repaintPath :: Board -> Transform -> Board
repaintPath board ((start, end), status) = updateBoard (end, status) $ updateBoard (start, E) board

--Movement Operators-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Game Rules: players move their pieces one after another based on turn, 
-- the first player that manages moving all of his pieces to the opposite corner of the start base wins the game
-- there are two movements allowed in the game: one is to simply jump to the adjacent empty position (step)
-- another is to jump over one occupied position to another empty position, called chained jump (or hops), 
-- this type of jump could be made multiple time for a larger distance, and at every turn only one of the jumps is allowed
-- besides, other players' bases are not allowed to stay or pass by
-- additionally, once a piece successfully enter the goal base, it cannot move out, but is still moveable within the base

-- offer a player's all avaiable moves on the given board
-- might cause error if contains multiple player's pieces
allDestinations :: [BoardPos] -> Board -> [Pos]
allDestinations bs board = setNub . concat $ parMap rseq (`destinations` board) bs

-- enter a board position and return a list of available movements/reachable positions: adjacent jump and chained jump
destinations :: BoardPos -> Board -> [Pos]
destinations bs@(ps, st) board = if isOccupied bs
                                 then let -- find the reachable destinations for steps
                                          js = jumps bs boardSize board
                                          -- find the reachable destinations for hops
                                          hs = allHops [] boardSize board st ps
                                      in  -- combine the two lists and discard the repeated ones
                                          js `par` hs `pseq` setNub (js ++ hs)
                                 else []  -- invalid chosen pieces

-- list the available position for a piece to move to
jumps :: BoardPos -> (Int, Int) -> Board -> [Pos]
jumps (bs, st) size board = let -- first list a serise of valid positions can be resulted from the board
                                moves = filter (validMove st size bs) (jumpDirections bs)
                                -- then check if such a position is occupied by another piece
                            in  filter (\key -> case HM.lookup key board of
                                                    Just E -> True
                                                    _ -> False) moves


-- list the directions of a piece
adjacentPositions :: [Pos -> Pos]
adjacentPositions = [f (-1, -1), f (0, -2), f (1, -1), f (1, 1), f (0, 2), f (-1, 1)]
    where
        f (a, b) (x, y) = (a+x, b+y)
-- list the adjacent positions for a piece to move to
jumpDirections :: Pos -> [Pos]
jumpDirections ps = adjacentPositions <*> pure ps

-- a valid movement of a piece: (start, end) should satisfy three points
-- first, the piece cannot exceed the board
-- second, the piece cannot access other players' bases
-- thrid, the piece cannot go out the goal base once it has entered
validMove :: Status -> (Int, Int) -> Pos -> Pos -> Bool
validMove st size start end = borderCheck size end && borderCheck2 st end && baseMoveAllow st start end
-- test if a position is not out of a set border
borderCheck :: (Int, Int) -> Pos -> Bool
borderCheck (xlimt, ylimt) (x, y) = x >= 0 && y >= 0 && x < xlimt && y < ylimt
-- an addition check, first project the board into a square occupied board for certain player and test whether it exists in that board
-- this is to ensure that the piece will not pass by other's bases
borderCheck2 :: Status -> Pos -> Bool
borderCheck2 st pos = borderCheck (occupiedBoardSize, occupiedBoardSize) (projection st pos)
-- besides, once a piece enter the goal base, it's not allowed to move out again, but could still move around within the base
baseMoveAllow :: Status -> Pos -> Pos -> Bool
baseMoveAllow st start end = let sp = projection st start
                                 ep = projection st end
                             in  (sp `notElem` goalBase) || (ep `elem` goalBase)

-- generate a list of one-over jump chained together to reach a larger jump range
-- recursively search for the reachable destinations for chained jumps of different directions
-- during the search, a list of discovered positions is maintained to avoid cycling/repetition, 
-- while another list stores the new frontiers based on the previous list of positions
allHops :: [Pos] -> (Int, Int) -> Board -> Status -> Pos -> [Pos]
allHops record size board st ps = -- generate a new list of positions found
                                  let moves = hops ps st size board
                                      -- add the newly discovered positions to the record, also avoid backward moves
                                      newGeneration = filter (`notElem` record) moves
                                      -- continue the next "layer" of search based on the positions discovered at this "layer"
                                      newRecord = newGeneration ++ record
                                      -- until not new positions are found, return the combined list of all found positions
                                      expandedMoves = concat $ parMap rseq (allHops newRecord size board st) newGeneration
                                      -- might exist duplicated positions, but will be omitted at the final combination
                                  in  expandedMoves ++ newGeneration

-- list a positions that can be reached by one hop
hops :: Pos -> Status -> (Int, Int) -> Board -> [Pos]
hops ps st size board = let -- generate a list of positions that could be reached by just one hop in different directions
                            moves = map (hopCheck ps st board size) adjacentPositions
                            -- discard the invalid positions
                        in  extractJustValues moves
    where
        extractJustValues [] = []
        extractJustValues (Just x:xs) = x:extractJustValues xs
        extractJustValues (Nothing:xs) = extractJustValues xs

-- given a board position, chek if a hop is valid in a certain direction
hopCheck :: Pos -> Status -> Board -> (Int, Int) -> (Pos -> Pos) -> Maybe Pos
hopCheck ps st board size f = if -- a hop means to jump over an adjancent piece, indicating that the first adjancent position 
                                 -- should be occupied and the following one should be empty  
                                 -- besides, a valid movement should also obey the above three rules
                                 isOccupied (HM.lookup ps1 board) && (HM.lookup ps2 board == Just E) && validMove st size ps ps2
                              then
                                 -- the moves that pass the test will be returned in Just
                                 Just ps2
                                 -- otherwise, will give Nothing
                              else Nothing
        where
            ps1 = f ps
            ps2 = f ps1

            isOccupied (Just E) = False
            isOccupied (Just U) = False
            isOccupied Nothing = False
            isOccupied _ = True
{-
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
-}
--Projection Operator----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- project the movement to the internal (occupied) board based on certain colour
projectMove :: Transform -> (Pos, Pos)
projectMove ((x, y), s) = (projection s x, projection s y)

-- the projection of the main board to the occupied board for each player (colour)
projection :: Status -> Pos -> Pos
projection G = projectGreen
projection B = projectBlue
projection P = projectPurple
projection R = projectRed
projection O = projectOrange
projection K = projectBlack
projection _ = error "Invalid Status for Projection"

-- revert the projected position to the main board ones
reversion :: Status -> Pos -> Pos
reversion G = reverseGreen
reversion B = reverseBlue
reversion P = reversePurple
reversion R = reverseRed
reversion O = reverseOrange
reversion K = reverseBlack
reversion _ = error "Invalid Status for Reversion"


