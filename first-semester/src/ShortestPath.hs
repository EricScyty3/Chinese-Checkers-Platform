module ShortestPath where
import Board
import Zobrist
import Data.List
import Data.Maybe


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

-- another search that counts the moves needed for reaching the goal state, mostly used for 
test :: OccupiedBoard
test = [
        [0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 1, 1],
        [0, 0, 0, 0, 0, 0, 1],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0]]

main = print (shortestMoves initialState 200)

-- breadth-first search
shortestMoves :: OccupiedBoard -> Int -> Int
shortestMoves b wd = bSearchS 0 wd [(findPieces b, centroid b)] []
-- for a list of board states, process each new state for each state 
-- update the new positions based on the old one
bSearchS :: Int -> Int -> [([Pos], Int)] -> [([Pos], Int)] -> Int
-- clear the new position list and start searching on them, and update the last level positions
bSearchS i wd np [] = bSearchS (i+1) wd [] np -- start searching at the next level
bSearchS i wd np (b:bs) = case evalState (bSearch np wd) b of
                             [(_, 28)] -> i  -- indicate that the goal state is reached, return the level
                             ls -> bSearchS i wd ls bs -- otherwise, keep searching
-- breath-first search, where each level is move that requires certain moves before can be performed
-- for each pieces on a board, return the resulting board states
-- bSearch :: [(OccupiedBoard, Int)] -> (OccupiedBoard, Int) -> Int -> [(OccupiedBoard, Int)]
-- bSearch np (b, s) bw = let ps = dListForBoard b -- then search for from-to pair for each piece
--                            nb = mirrorCheck $ flipLists b s ps -- retrieve the resulting new board states containing no mirror images
--                        in  updateList np nb bw -- return the updated new positions list
bSearch :: [([Pos], Int)] -> Int -> State ([Pos], Int) [([Pos], Int)]
bSearch np wd = do (board, score) <- get
                   let moves = dListForBoard board
                       boardsAndScores = evalState (flipLists score moves) board
                   {-let uniqueList = mirrorCheck bs-}
                   return (updateList np boardsAndScores wd)

-- update the new positions list with better positions of fixed length
updateList :: [([Pos], Int)] -> [([Pos], Int)] -> Int -> [([Pos], Int)]
updateList os [] _ = os
updateList os (n:ns) bw
  | snd n == 28 = [n] -- if reach the goal then just return the flag 
  | n `elem` os = updateList os ns bw -- skip if already exists
  | length os < bw = updateList (n:os) ns bw -- just add if the breadth is not wide enough
  | otherwise = let minScore = minimum (map snd os)
                    (_, score) = n
                in  if score > minScore then let idx = head (elemIndices minScore (map snd os))
                                                 newList = runST $ do ls <- newSTRef os
                                                                      modifySTRef ls (replace idx n)
                                                                      readSTRef ls
                                             in  updateList newList ns bw -- replace the element with the current board state
                    else updateList os ns bw -- if the board state is not better then skip 

-- to tend the movement from right-top to left-bottom, the centroid of the position can be set as: y-x
-- such that the the closer to the homebase, the larger the centroid is
-- also, considering the symmetric of the board across the diagonal line, hence, the search space could be halved 
-- centroid :: OccupiedBoard -> Int
-- centroid oboard = centroidOfBoard oboard 0 0
-- -- calculate for the whole occupied board
centroid :: OccupiedBoard -> Int
centroid board = let ps = findPieces board
                 in  sum (map centroidPos ps)
-- centroidOfBoard _ _ 7 = 0
-- centroidOfBoard oBoard 7 y = centroidOfBoard oBoard 0 (y+1)
-- centroidOfBoard oboard x y = if getElement oboard (x, y) == 1 then centroidPos (x, y) + centroidOfBoard oboard (x+1) y
--                              else centroidOfBoard oboard (x+1) y
-- -- the heuristic evaluation to estimate the board and detect the win state
centroidPos :: Pos -> Int
centroidPos (x, y) = y - x
-- the two types of symmetric features
symmetric1 :: OccupiedBoard -> OccupiedBoard
symmetric1 = reverse . transpose . reverse
symmetric2 :: OccupiedBoard -> OccupiedBoard
symmetric2 = transpose

-- should avoid implementing mirror/symmetric state to reduce the search space
-- diagonal line from top right to left bottom
-- mirrorCheck :: [(OccupiedBoard, Int)] -> [(OccupiedBoard, Int)]
-- mirrorCheck [] = []
-- mirrorCheck (x:xs) = let (board, _) = x
--                          mirrorBoard = symmetric1 board
--                      in  if mirrorBoard `elem` map fst xs then mirrorCheck xs
--                          else x:mirrorCheck xs

-- implement the list of movements and the resulting board states
flipLists :: Int -> [(Pos, [Pos])] -> State [Pos] [([Pos], Int)]
flipLists _ [] = return []
flipLists c (x:xs) = do let (p, ps) = x
                        resultingBoards <- mapM (flipBoardState c p) ps
                        otherResultingBoards <- flipLists c xs
                        return(resultingBoards `par` otherResultingBoards `pseq` (resultingBoards ++ otherResultingBoards))
    where
        -- exchange two pieces' states on the occupied board
        -- flipBoardState :: OccupiedBoard -> Int -> Pos -> Pos -> (OccupiedBoard, Int)
        -- flipBoardState b c f t = let new1 = replace2 f (flip $ getElement b f) b
        --                              new2 = replace2 t (flip $ getElement new1 t) new1
        --                          in  (new2, c - centroidPos f + centroidPos t)
        flipBoardState :: Int -> Pos -> Pos -> State [Pos] ([Pos], Int)
        flipBoardState c f t = do ps <- get
                                  case elemIndex f ps of
                                    Nothing -> error "Cannot find this position in the occupied board"
                                    Just i  -> do let newps = runST $ do n <- newSTRef ps
                                                                         modifySTRef n (replace i t)
                                                                         readSTRef n
                                                  return (newps, c - centroidPos f + centroidPos t)

{-
    -- provide the available pieces and their avaliable movements


    -- A similar movements list should be provided, but this time in occupied board with binary results
    destinationList' :: OccupiedBoard -> Pos -> [Pos]
    destinationList' oBoard p = nub $ findAvaliableNeighbors' oBoard p ++ searchWithoutLooping' oBoard [] p
    -- stepping to an adjacent non-occupied position
    findAvaliableNeighbors' :: OccupiedBoard -> Pos -> [Pos]
    findAvaliableNeighbors' oBoard p = filter (not . testOccupiedPos' oBoard) (findValidNeighbors' p oBoard)
    -- search for the valid adjacent positions
    findValidNeighbors' :: Pos -> OccupiedBoard -> [Pos]
    findValidNeighbors' (x, y) oBoard = filter testValidPos' [(x, y-1), (x-1, y-1), (x-1, y), (x, y+1), (x+1, y+1), (x+1, y)]
    -- test the occupied state for a piece

    -- test if a piece is inside the board

    -- the moveable positions for chained hops in occupired board
    searchWithoutLooping' :: OccupiedBoard -> [Pos] -> Pos ->  [Pos]
    searchWithoutLooping' oBoard l p = let s = jumpToAllDirections' oBoard p
                                        renewList = filter (`notElem` l) s
                                        recordList = renewList ++ l
                                    in  concatMap (searchWithoutLooping' oBoard recordList) renewList ++ renewList
    -- six directions' hops
    jumpToAllDirections' :: OccupiedBoard -> Pos -> [Pos]
    jumpToAllDirections' oBoard pos = filter (/= pos) (jumpToOneDirection' oBoard pos [(0, -1), (-1, -1), (-1, 0), (0, 1), (1, 1), (1, 0)])
    -- one direction's hop
    jumpToOneDirection' :: OccupiedBoard -> Pos -> [Pos] -> [Pos]
    jumpToOneDirection' _ _ [] = []
    jumpToOneDirection' oBoard pos (a:as) = determineValidJump' oBoard pos (f a): jumpToOneDirection' oBoard pos as
        where
            f (a, b) (x, y) = (a+x, b+y)
    -- check the chained hops' validity
    determineValidJump' :: OccupiedBoard -> Pos -> (Pos -> Pos) -> Pos
    determineValidJump' oBoard pos f
        | not (testValidPos' (f pos)) || not (testValidPos' ((f . f) pos)) = pos -- invalid ones
        | testOccupiedPos' oBoard (f pos) && not (testOccupiedPos' oBoard ((f . f) pos)) = (f . f) pos
        | otherwise = pos -- no ways found
-}

dListForBoard :: [Pos] -> [(Pos, [Pos])]
dListForBoard ps = evalState (mapM destinationList' ps) ps -- zip the from and to destinations

-- find the occupied positions of the board/find the pieces' positions on the board
findPieces :: OccupiedBoard -> [Pos]
findPieces board = [(x, y) | (y, row) <- zip [0..] board, x <- elemIndices 1 row]

destinationList' :: Pos -> State [Pos] (Pos, [Pos])
destinationList' p = do ps <- get
                        if p `notElem` ps then return (p, [])
                        else do adjacentMoves <- findAvaliableNeighbors' p
                                chainedMoves  <- recursiveSearch' [] p
                                let movesList = adjacentMoves `par` chainedMoves `pseq` nubOrd (adjacentMoves ++ chainedMoves)
                                return (p, movesList)

-- state monad version
findAvaliableNeighbors' :: Pos -> State [Pos] [Pos]
findAvaliableNeighbors' (x, y) = do ps <- get
                                    let avaliableList = filter (`notElem` ps) neighborPosList
                                    return avaliableList
    where
        neighborPosList = filter testValidPos' [(x, y-1), (x-1, y-1), (x-1, y), (x, y+1), (x+1, y+1), (x+1, y)]

recursiveSearch' :: [Pos] -> Pos -> State [Pos] [Pos]
recursiveSearch' ls pos = do chainJumpList <- jumpDirection' pos
                             let renewList = filter (`notElem` ls) chainJumpList
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

-- don't really need the whole occupied board, only need to known about the occupied positions, hence, [Pos]
determineValidJump' :: Pos -> (Pos -> Pos) -> State [Pos] Pos
determineValidJump' pos f = do if not (testValidPos' fp) || not (testValidPos' fp2) then return pos
                               else do ps <- get
                                       if fp2 `notElem` ps && fp `elem` ps then return fp2
                                       else return pos
    where
        fp = f pos
        fp2 = (f . f) pos

testValidPos' :: Pos -> Bool
testValidPos' (x, y) = x >= 0 && y >= 0 && x < occupiedBoardSize && y < occupiedBoardSize