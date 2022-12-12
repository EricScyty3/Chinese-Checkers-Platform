module BFS where

import Board
import Zobrist
import Data.List
import Control.Monad.ST
import Data.STRef
import Control.Monad.State
import Control.Monad.Extra
import Control.Parallel
import Data.Containers.ListUtils




-- basically will need a breadth-first search for preventing duplicate moves, it is known that 28 is the highest value
-- each level represents a move that is done only if certain moves were done
-- level 0 means the initial positions
-- level 1 means a step or jump performed by each piece from the initial positions, might have duplicate moves

-- Count iterations: moves needed to reaching goal state
-- Copy all the "new" board positions to the "old" board positions: level plus
--      For each of the "old" board positions
--          For each of the 10 marbles in the current "old" position
--              For each possible move of each of these marbles
--                  Calculate the "Score" for the resulting new positions and update the new positions list

testBoard :: OccupiedBoard
testBoard = [
        -- [0,1,0,1,1,0,1],
        -- [0,0,0,0,0,0,0],
        -- [0,0,0,0,0,0,0],
        -- [0,0,0,0,0,0,1],
        -- [0,0,0,0,0,0,0],
        -- [0,0,0,0,0,0,1],
        -- [0,0,0,0,0,0,0]
        [0,1,0,0,1,1,0],
        [0,0,0,1,0,0,0],
        [0,0,0,0,0,0,1],
        [0,0,0,0,0,0,1],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0]
    ]
-- additional consideration for more accurate estimate
-- breadth-first search
-- when searching, don't really need the whole occupied board, only need to known about the occupied positions
-- very similar to the board search, but with different a move strategy
shortestMoves :: OccupiedBoard -> Int -> Int
shortestMoves b wd = if centroid b == 28 then 0 else bSearchS 0 wd [(findOccupiedPieces b, centroid b)] []



-- for a list of board states, process each new state for each state 
-- update the new positions based on the old one
-- the arguments including the level, breadth width, the new found states, and the currently explored states
-- and will finally return the level index, meaning the moves requried to reach the goal state
bSearchS :: Int -> Int -> [([Pos], Int)] -> [([Pos], Int)] -> Int
-- clear the new position list and start searching on them, and update the last level positions
bSearchS i wd np [] = bSearchS (i+1) wd [] np -- start searching at the next level
bSearchS i wd np (b:bs) = case evalState (bSearch np wd) b of
                             [(_, 28)] -> i  -- indicate that the goal state is reached, return the level
                             ls -> bSearchS i wd ls bs -- otherwise, keep searching with the new found states


-- for each pieces on a board, return the resulting board states
-- return the list containing the new found states and previously accumulated states
bSearch :: [([Pos], Int)] -> Int -> State ([Pos], Int) [([Pos], Int)]
bSearch ps wd = do (board, score) <- get
                   let moves = dListForBoard board
                       ns = evalState (flipLists score moves) board 
                       newPositionsList = runST $ do n <- newSTRef ps
                                                     modifySTRef n (updateList ns wd)
                                                     readSTRef n
                   return newPositionsList

-- since A star is not as accurate as expected, try to improve with different data structure here
updateList :: [([Pos], Int)] -> Int -> [([Pos], Int)] -> [([Pos], Int)]
updateList [] _ ps = ps
updateList (n:ns) wb ps
  | snd n == 28 = [n] -- if reach the goal then just return the goal state
  | n `elem` ps = updateList ns wb ps -- skip if already exists, the mirror image could be added due to the large search range
  | length ps < wb = updateList ns wb (mySort (n:ps)) -- just add if the search wide is not large as expected
  | otherwise = let minScore = (snd . head) ps -- the minimum item is the first item of the list
                    (_, score) = n
                in  if score > minScore then updateList ns wb (mySort (n:tail ps)) -- replace the element with the current board state
                    else updateList ns wb ps -- if the board state is not better then skip 

-- sort the list based on score, such that every time the minimum value is the head
mySort :: [([Pos], Int)] -> [([Pos], Int)]
mySort = sortBy (\(_, x) (_, y) -> compare x y)

-- to tend the movement from right-top to left-bottom, the centroid of the position can be set as: y-x
-- such that the the closer to the homebase, the larger the centroid is
-- also, considering the symmetric of the board across the diagonal line, hence, the search space could be halved 
-- -- calculate for the whole occupied board
centroid :: OccupiedBoard -> Int
centroid board = let ps = findOccupiedPieces board
                 in  sum (map centroidPos ps)
-- -- the heuristic evaluation to estimate the board and detect the win state
centroidPos :: Pos -> Int
centroidPos (x, y) = y - x
-- the two types of symmetric features
symmetric1 :: OccupiedBoard -> OccupiedBoard
symmetric1 = reverse . transpose . reverse
symmetric2 :: OccupiedBoard -> OccupiedBoard
symmetric2 = transpose

symmetric2_pos :: [Pos] -> [Pos]
symmetric2_pos = map (\(x, y) -> (6 - y, 6 - x))

mirrorImage :: ([Pos], Int) -> ([Pos], Int)
mirrorImage (ps, c) = let sp = sort $ symmetric2_pos ps
                      in  (sp, c)


-- implement the list of movements and the resulting board states
flipLists :: Int -> [(Pos, [Pos])] -> State [Pos] [([Pos], Int)]
flipLists _ [] = return []
flipLists c (x:xs) = do let (p, ps) = x
                        resultingBoards <- mapM (flipBoardState c p) ps
                        otherResultingBoards <- flipLists c xs
                        return(resultingBoards `par` otherResultingBoards `pseq` (resultingBoards ++ otherResultingBoards))
    where
        -- exchange two pieces' states on the occupied board
        flipBoardState :: Int -> Pos -> Pos -> State [Pos] ([Pos], Int)
        flipBoardState c f t = do ps <- get
                                  case elemIndex f ps of
                                    Nothing -> error "Cannot find this position in the occupied board"
                                    Just i  -> do let newps = replace i t ps
                                                  return (sort newps, c - centroidPos f + centroidPos t)

-- only need to consider the positions of the pieces since the board is simpler
dListForBoard :: [Pos] -> [(Pos, [Pos])]
dListForBoard ps = evalState (mapM destinationList' ps) ps -- zip the from and to destinations

-- combine the two move lists
destinationList' :: Pos -> State [Pos] (Pos, [Pos])
destinationList' p = do ps <- get
                        if p `notElem` ps then return (p, [])
                        else do adjacentMoves <- findAvaliableNeighbors' p
                                chainedMoves  <- recursiveSearch' [] p
                                let movesList = adjacentMoves `par` chainedMoves `pseq` nubOrd (adjacentMoves ++ chainedMoves)
                                return (p, movesList)

findAvaliableNeighbors' :: Pos -> State [Pos] [Pos]
findAvaliableNeighbors' (x, y) = do ps <- get
                                    let avaliableList = filter (`notElem` ps) neighborPosList
                                    return avaliableList
    where
        neighborPosList = filter (testValidPos size size) [(x, y-1), (x-1, y-1), (x-1, y), (x, y+1), (x+1, y+1), (x+1, y)]
        size = occupiedBoardSize

recursiveSearch' :: [Pos] -> Pos -> State [Pos] [Pos]
recursiveSearch' ls pos = do chainJumpList <- jumpDirection' pos
                             let renewList = filter (`notElem` ls) chainJumpList -- prevent duplicate search 
                                 recordList = renewList ++ ls
                             recursiveList <- concatMapM (recursiveSearch' recordList) renewList
                             return (recursiveList ++ renewList)

-- state monad version 
jumpDirection' :: Pos -> State [Pos] [Pos]
jumpDirection' pos = do reachableList <- mapM (determineValidJump' pos) [f (0, -1), f (-1, -1), f (-1, 0), f (0, 1), f (1, 1), f (1, 0)]
                        let validReachableList = filter (/= pos) reachableList -- remove the invalid moves
                        return validReachableList
    where
        f (a, b) (x, y) = (a+x, b+y)

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

