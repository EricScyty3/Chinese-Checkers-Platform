module BFS where
-- in order to complete a lookup table for evaluating the board state, the minimum moves for each board to reach the goal state is necessary
-- hence, this module aims to search the sufficient moves of any state for a player to win the game in single-agent board
import Board
import Zobrist
import Data.List
import Control.Monad.ST
import Data.STRef
import Control.Monad.State
import Control.Monad.Extra
import Control.Parallel
import Data.Containers.ListUtils


-- basically will need a breadth-first search for preventing duplicate moves, and also, due to the difficulty of measuring the prediction in A star, BFS is somehow more accurate 
-- each level represents a move that is done only if certain moves before were done
-- level 0 means the initial state
-- level 1 means a move is performed by each piece from the initial state, might exists duplications

-- main = print $ shortestMoves testBoard 800

testBoard :: OccupiedBoard
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
-- additional consideration for more accurate estimate
-- breadth-first search
-- when searching, don't really need the whole occupied board, only need to known about the occupied positions
shortestMoves :: [Pos] -> Int -> Int
shortestMoves ps wd = let evalValue = centroid ps
                      in  if evalValue == 28 then 0 else bSearchS 0 wd [(sort ps, evalValue)] [] []

-- for a list of board states, process each new state for each state 
-- update the new positions based on the old ones
-- finally return the level index, meaning the moves requried to reach the goal state
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
                               in  if set1 `par` set2 `pseq` goalReached set1 || goalReached set2 then i
                                   else bSearchS i wd set1 set2 (init bs)
    where
        goalReached :: [([Pos], Int)] -> Bool
        goalReached [(_, 28)] = True
        goalReached _ = False

-- for each pieces on a board, return the resulting board states it could lead to
-- return the list containing the new found states and previously accumulated states
bSearch :: [([Pos], Int)] -> Int -> State ([Pos], Int) [([Pos], Int)] -- maintaining the current board and its value
bSearch ps wd = do (board, score) <- get
                   let moves = dListForBoard board -- retrieve the avaliable moves
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
                in  if score `par` minScore `pseq` score > minScore then updateList ns wb (mySort (n:tail ps)) -- replace the element with the current board state
                    else updateList ns wb ps -- if the board state is not better then skip 

-- sort the list based on score, such that every time the minimum value is at the front
mySort :: [([Pos], Int)] -> [([Pos], Int)]
mySort = sortBy (\(_, x) (_, y) -> compare x y)

--Board Handleing--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- to tend the movement from right-top to left-bottom, the centroid of the position can be set as: y-x
-- the centroid is used as the board evaluation for the shortest path search that the closer it is to the goal state, the larger reflect is given
-- such that the the closer to the homebase, the larger the centroid is
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
                        return(resultingBoards `par` otherResultingBoards `pseq` (resultingBoards ++ otherResultingBoards)) -- eventually get all possible boards result from the current board
    where
        -- exchange two pieces' states on the occupied board by replacing the position with a new one as well as update the new evaluation value
        flipBoardState :: Int -> Pos -> Pos -> State [Pos] ([Pos], Int)
        flipBoardState v f t = do ps <- get
                                  case elemIndex f ps of
                                    Nothing -> error "Cannot find this position in the occupied board"
                                    Just i  -> do let newps = sort $ t : filter (/= f) ps
                                                      newv  = v - centroidPos f + centroidPos t
                                                  return (newps `par` newv `pseq` (newps, newv))

--Movement Operators-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- similar to the Board module but with different movement formats and the board is simpler
-- given a list of positions, find their corresponding avaliable movements
dListForBoard :: [Pos] -> [(Pos, [Pos])]
dListForBoard ps = evalState (mapM destinationList' ps) ps -- zip the from and to destinations

-- combine the two movement lists and discard the duplicate ones
destinationList' :: Pos -> State [Pos] (Pos, [Pos])
destinationList' p = do ps <- get
                        if p `notElem` ps then return (p, []) -- invalud input position
                        else do adjacentMoves <- findAvaliableNeighbors' p
                                chainedMoves  <- recursiveSearch' [] p
                                let movesList = adjacentMoves `par` chainedMoves `pseq` nubOrd (adjacentMoves ++ chainedMoves)
                                return (p, movesList) -- return the pair of source positions and a list of target positions

-- discover the avaliable adjacent positions arround the entered one
findAvaliableNeighbors' :: Pos -> State [Pos] [Pos]
findAvaliableNeighbors' (x, y) = do ps <- get
                                    let avaliableList = filter (`notElem` ps) neighborPosList
                                    return avaliableList
    where
        neighborPosList :: [Pos]
        neighborPosList = filter (testValidPos size size) [(x, y-1), (x-1, y-1), (x-1, y), (x, y+1), (x+1, y+1), (x+1, y)]
        size = occupiedBoardSize

-- search for all chained jump destinations that can be reached
recursiveSearch' :: [Pos] -> Pos -> State [Pos] [Pos]
recursiveSearch' record pos = do chainJumpList <- jumpDirection' pos
                                 let renewList = filter (`notElem` record) chainJumpList -- prevent duplicate search 
                                     newRecord = renewList ++ record
                                 recursiveList <- concatMapM (recursiveSearch' newRecord) renewList
                                 return (recursiveList ++ renewList)

-- return the avaliable hopping destinations
jumpDirection' :: Pos -> State [Pos] [Pos]
jumpDirection' pos = do reachableList <- mapM (determineValidJump' pos) [f (0, -1), f (-1, -1), f (-1, 0), f (0, 1), f (1, 1), f (1, 0)]
                        return $ filter (/= pos) reachableList -- remove the invalid moves
    where
        f (a, b) (x, y) = (a+x, b+y)

-- check if a one over hop is valid
determineValidJump' :: Pos -> (Pos -> Pos) -> State [Pos] Pos
determineValidJump' pos f = do if not (testValidPos size size fp) ||
                                  not (testValidPos size size fp2) then return pos
                               else do ps <- get
                                       if fp2 `notElem` ps && fp `elem` ps then return fp2
                                       else return pos
    where
        fp = f pos
        fp2 = (f . f) pos
        size = occupiedBoardSize

