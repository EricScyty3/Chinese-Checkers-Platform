module NBFS where
-- in order to complete a lookup table for evaluating the board state, the minimum moves for each board to reach the goal state is necessary
-- hence, this module aims to search the sufficient moves of any state for a player to win the game in single-agent board
-- if you're not interested in how to retrieve the lookup table used in this application, you can ignore this module

import NBoard ( occupiedBoardSize, Pos, borderCheck, extractJustValues, setNub )
import Data.List ( elemIndex, sort, sortBy, transpose )
import Control.Monad.ST ( runST )
import Data.STRef ( modifySTRef, newSTRef, readSTRef )
import Control.Monad.State ( State, evalState, MonadState(get) )
import Control.Monad.Extra ( concatMapM )
import Control.Parallel ( par, pseq )
import Data.Containers.ListUtils ( nubOrd )
import NProjection (goalBase, startBase)
import Control.Parallel.Strategies (parMap, rseq)

-- it will need a breadth-first search for preventing duplicate moves, and also, due to the difficulty of measuring the prediction in A star, BFS is somehow more accurate 
-- each level represents a move that is done only if certain moves before were done
-- level 0 means the initial state
-- level 1 means a move is performed by each piece from the initial state, might exists duplications
-- as shown below:
testBoard :: [[Int]]
testBoard = [
        [0,1,0,0,1,1,0],
        [0,0,0,1,0,0,0],
        [0,0,0,0,0,0,1],
        [0,0,0,0,0,0,1],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0]
    ]
-- to speed up the processing, rather than having the whole occupied board, only the occupied positions are maintained in the state monad
-- additional consideration for more accurate estimate might be considered in multi-agent form, but not necessary here

{-

-- breadth-first search
shortestMoves :: [Pos] -> Int -> Int
shortestMoves ps wd = let evalValue = centroid ps
                      in  if evalValue == 28 then 0 else bSearchS 0 wd [(sort ps, evalValue)] [] []

-- for a list of board states, process each new state for each state 
-- update the new positions based on the old ones
-- finally return the level index, meaning the moves required to reach the goal state
bSearchS :: Int -> Int -> [([Pos], Int)] -> [([Pos], Int)] -> [([Pos], Int)] -> Int
bSearchS i wd np1 np2 [] = let combinedSet = runST $ do n <- newSTRef np2
                                                        modifySTRef n (updateList np1 wd)
                                                        readSTRef n
                           in bSearchS (i+1) wd [] [] combinedSet -- clear the new position list and start searching on them, and update the level counts

bSearchS i wd np1 np2 [b] = case evalState (bSearch np1 wd) b of
                                [(_, 28)] -> i  -- indicate that the goal state is reached, return the level counts
                                ls -> bSearchS i wd ls np2 [] -- otherwise, keep searching/expanding the known board states

bSearchS i wd np1 np2 (b:bs) = let set1 = evalState (bSearch np1 wd) b -- bidirectional solution, that solves the boards in two direction in parallel to save the time
                                   set2 = evalState (bSearch np2 wd) (last bs)
                               in  if set1 `par` set2 `pseq` (goalReached set1 || goalReached set2) then i
                                   else bSearchS i wd set1 set2 (init bs)
    where
        goalReached :: [([Pos], Int)] -> Bool
        goalReached [(_, 28)] = True
        goalReached _ = False

-- for each pieces on a board, return the resulting board states it could lead to
-- return the list containing the new found states and previously accumulated states
bSearch :: [([Pos], Int)] -> Int -> State ([Pos], Int) [([Pos], Int)] -- maintaining the current board and its value
bSearch ps wd = do (board, score) <- get
                   let moves = dListForBoard board -- retrieve the available moves
                       ns = evalState (flipLists score moves) board -- result the expanded boards
                   return (runST $ do n <- newSTRef ps
                                      modifySTRef n (updateList ns wd)
                                      readSTRef n) -- update the previously discovered board states with the new found board states

-- popular the candidate movements within a certain width, also replace the board states with better ones once the width is reached
updateList :: [([Pos], Int)] -> Int -> [([Pos], Int)] -> [([Pos], Int)]
updateList [] _ ps = ps -- return the new candidate set
updateList (n:ns) wb ps
  | snd n == 28 = [n] -- if reach the goal then just return the goal state
  | n `elem` ps = updateList ns wb ps -- skip if already exists in the candidate set
  | length ps < wb = updateList ns wb (mySort (n:ps)) -- just add if still has space
  | otherwise = let minScore = (snd . head) ps -- the minimum item is the first item of the list
                    (_, score) = n
                in  if score `par` minScore `pseq` (score > minScore) then updateList ns wb (mySort (n:tail ps)) -- replace the element with the current board state
                    else updateList ns wb ps -- if the board state is not better then skip 

-- sort the list based on score, such that every time the minimum value is at the front
mySort :: [([Pos], Int)] -> [([Pos], Int)]
mySort = sortBy (\(_, x) (_, y) -> compare x y)

--Board Handling--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- to tend the movement from right-top to left-bottom, the centroid of the position can be set as: y-x
-- the centroid is used as the board evaluation for the shortest path search that the closer it is to the goal state, the larger reflect is given
-- such that the the closer to the home base, the larger the centroid is
-- calculate the centroid for the whole occupied board
centroid :: [Pos] -> Int
centroid ps = sum (map centroidPos ps) -- the highest value will be 28 while the lowest is -28
centroidPos :: Pos -> Int
centroidPos (x, y) = y - x

-- the two types of symmetric a board could be presented
symmetric1 :: OccupiedBoard -> OccupiedBoard
symmetric1 = reverse . transpose . reverse
symmetric2 :: OccupiedBoard -> OccupiedBoard
symmetric2 = transpose
-- the symmetric conversion based on occupied positions 
symmetric1_pos :: [Pos] -> [Pos]
symmetric1_pos = map (\(x, y) -> (6 - y, 6 - x))
symmetric2_pos :: [Pos] -> [Pos]
symmetric2_pos = map (\(x, y) -> (y, x))

-- implement the list of movements and the resulting boards (positions)
-- as well as update the new evaluation value of the resulting board
flipLists :: Int -> [(Pos, [Pos])] -> State [Pos] [([Pos], Int)]
flipLists _ [] = return []
flipLists v (x:xs) = do let (p, ps) = x
                        resultingBoards <- mapM (flipBoardState v p) ps -- get the resulting boards and their values for the one position
                        otherResultingBoards <- flipLists v xs -- the rest of the resulting boards as well as the corresponding values
                        return (resultingBoards `par` otherResultingBoards `pseq` (resultingBoards ++ otherResultingBoards)) -- eventually get all possible boards result from the current board
    where
        -- exchange two pieces' states on the occupied board by replacing the position with a new one as well as update the new evaluation value
        flipBoardState :: Int -> Pos -> Pos -> State [Pos] ([Pos], Int)
        flipBoardState v f t = do ps <- get
                                  case elemIndex f ps of
                                    Nothing -> error "Cannot find this position in the occupied board"
                                    Just i  -> do let newps = sort $ t : filter (/= f) ps
                                                      newv  = v - centroidPos f + centroidPos t
                                                  return (newps `par` newv `pseq` (newps, newv))
-}
--Movement Operators-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- similar to the Board module but with different formats (Pos) and the board is simpler since there is no need to consider the colour issues
-- given a list of positions, find their corresponding available movements

-- dListForBoard :: [Pos] -> [(Pos, [Pos])]
-- dListForBoard ps = evalState (mapM destinationList' ps) ps -- zip the from and to destinations

-- offer a (only one) player's all avaiable moves on the given board
-- might cause error if contains multiple player's pieces
allDestinations' :: [Pos] -> [Pos]
allDestinations' board = setNub . concat $ parMap rseq (`destinations'` board) board

-- enter a board position and return a list of available movements/reachable positions: adjacent jump and chained jump
destinations' :: Pos -> [Pos] -> [Pos]
destinations' pos board = if pos `elem` board
                          then let -- find the reachable destinations for steps
                                   js = jumps' pos board
                                   -- find the reachable destinations for hops
                                   hs = allHops' [] board pos
                               in  -- combine the two lists and discard the repeated ones
                                   js `par` hs `pseq` setNub (js ++ hs)
                          else []  -- invalid chosen pieces

-- -- combine the two movement lists and discard the duplicate ones
-- destinationList' :: Pos -> State [Pos] (Pos, [Pos])
-- destinationList' p = do ps <- get
--                         if p `notElem` ps then return (p, []) -- invalid input position
--                         else do adjacentMoves <- findAvaliableNeighbors' p
--                                 chainedMoves  <- recursiveSearch' [] p
--                                 let movesList = adjacentMoves `par` chainedMoves `pseq` nubOrd (adjacentMoves ++ chainedMoves)
--                                 return (p, movesList) -- return the pair of source positions and a list of target positions

adjacentPositions' :: [Pos -> Pos]
adjacentPositions' = [f (-1, 0), f (-1, -1), f (0, -1), f (1, 0), f (1, 1), f (0, 1)]
    where
        f (a, b) (x, y) = (a+x, b+y)

jumpDirections' :: Pos -> [Pos]
jumpDirections' ps = adjacentPositions' <*> pure ps

-- discover the available adjacent positions around the entered one
jumps' :: Pos -> [Pos] -> [Pos]
jumps' pos board = filter (\x -> x `notElem` board && validMove' pos x) (jumpDirections' pos) 

-- -- search for all chained jump destinations that can be reached
-- recursiveSearch' :: [Pos] -> Pos -> State [Pos] [Pos]
-- recursiveSearch' record pos = do chainJumpList <- jumpDirection' pos
--                                  let renewList = filter (`notElem` record) chainJumpList -- prevent duplicate search 
--                                      newRecord = renewList ++ record
--                                  recursiveList <- concatMapM (recursiveSearch' newRecord) renewList
--                                  return (recursiveList ++ renewList)

-- generate a list of one-over jump chained together to reach a larger jump range
-- recursively search for the reachable destinations for chained jumps of different directions
-- during the search, a list of discovered positions is maintained to avoid cycling/repetition, 
-- while another list stores the new frontiers based on the previous list of positions
allHops' :: [Pos] -> [Pos] -> Pos -> [Pos]
allHops' record board ps = -- generate a new list of positions found
                           let moves = hops' ps board
                               -- add the newly discovered positions to the record, also avoid backward moves
                               newGeneration = filter (`notElem` record) moves
                               -- continue the next "layer" of search based on the positions discovered at this "layer"
                               newRecord = newGeneration ++ record
                               -- until not new positions are found, return the combined list of all found positions
                               expandedMoves = concat $ parMap rseq (allHops' newRecord board) newGeneration
                              -- might exist duplicated positions, but will be omitted at the final combination
                           in  expandedMoves ++ newGeneration

-- list a positions that can be reached by one hop
hops' :: Pos -> [Pos] -> [Pos]
hops' ps board = let -- generate a list of positions that could be reached by just one hop in different directions
                     moves = map (hopCheck' ps board) adjacentPositions'
                     -- discard the invalid positions
                 in  extractJustValues moves

-- -- return the available hopping destinations
-- jumpDirection' :: Pos -> State [Pos] [Pos]
-- jumpDirection' pos = do reachableList <- mapM (determineValidJump' pos) [f (0, -1), f (-1, -1), f (-1, 0), f (0, 1), f (1, 1), f (1, 0)]
--                         return $ filter (/= pos) reachableList -- remove the invalid moves
--     where
--         f (a, b) (x, y) = (a+x, b+y)

-- given a board position, chek if a hop is valid in a certain direction
hopCheck' :: Pos -> [Pos] -> (Pos -> Pos) -> Maybe Pos
hopCheck' pos board f = if isOccupied ps1 && not (isOccupied ps2) && validMove' pos ps2 then Just ps2 else Nothing
        where
            ps1 = f pos
            ps2 = f ps1

            isOccupied x = x `elem` board

-- check if a one over hop is valid
-- determineValidJump' :: Pos -> (Pos -> Pos) -> State [Pos] Pos
-- determineValidJump' pos f = if not validMoveCheck then return pos
--                             else do ps <- get
--                                     if fp2 `notElem` ps && fp `elem` ps then return fp2
--                                     else return pos
--     where
--         fp = f pos
--         fp2 = (f . f) pos
--         size = occupiedBoardSize

--         -- check the validity of the movement
--         validMoveCheck :: Bool
--         validMoveCheck = testValidPos size size fp2 && baseMoveAllow' pos fp2

-- check the validity of the movement
validMove' :: Pos -> Pos -> Bool
validMove' from to = borderCheck' to && baseMoveAllow' from to

borderCheck' :: Pos -> Bool
borderCheck' = NBoard.borderCheck (occupiedBoardSize, occupiedBoardSize)

-- prevent piece from moving out the goal base
baseMoveAllow' :: Pos -> Pos -> Bool
baseMoveAllow' from to = (from `notElem` goalBase) || (to `elem` goalBase)
