module ShortestPath where
import Board
import Zobrist
import Data.List
import Data.Maybe

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
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [1, 0, 0, 0, 0, 0, 0],
        [0, 1, 1, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [1, 1, 0, 0, 0, 0, 0],
        [1, 0, 0, 0, 0, 0, 0]]

--combine with 

-- breadth-first search
shortestMoves :: OccupiedBoard -> Int -> Int
shortestMoves b bw = bSearchS 0 bw [(b, centroid b)] []

-- for a list of board states, process each new state for each state 
-- update the new positions based on the old one
bSearchS :: Int -> Int -> [(OccupiedBoard, Int)] -> [(OccupiedBoard, Int)] -> Int
-- clear the new position list and start searching on them, and update the last level positions
bSearchS i bw np [] = bSearchS (i+1) bw [] np -- start searching at the next level
bSearchS i bw np (b:bs) = case bSearch np b bw of
                             [(_, 100)] -> i  -- indicate that the goal state is reached, return the level
                             ls -> bSearchS i bw ls bs -- otherwise, keep searching

-- breath-first search, where each level is move that requires certain moves before can be performed
-- for each pieces on a board, return the resulting board states
bSearch :: [(OccupiedBoard, Int)] -> (OccupiedBoard, Int) -> Int -> [(OccupiedBoard, Int)]
bSearch np (b, s) bw = let ps = dListForBoard b -- then search for from-to pair for each piece
                           nb = mirrorCheck $ flipLists b s ps -- retrieve the resulting new board states containing no mirror images
                       in  updateList np nb bw -- return the updated new positions list

-- update the new position list with better positions of fixed length
updateList :: [(OccupiedBoard, Int)] -> [(OccupiedBoard, Int)] -> Int -> [(OccupiedBoard, Int)]
updateList os [] _ = os
updateList os (n:ns) bw
  | snd n == 28 = [(fst n, 100)] -- if reach the goal then just return the result 
  | n `elem` os = updateList os ns bw -- skip if already exists
  | length os < bw = updateList (n:os) ns bw -- just add if the breadth is not wide enough
  | otherwise = let minScore = minimum (map snd os)
                    (_, score) = n
                in  if score > minScore then let idx = fromMaybe 0 (elemIndex minScore (map snd os))
                                             in  updateList (replace idx n os) ns bw -- replace the element with the current board state
                    else updateList os ns bw -- if the board state is not better then skip 


-- to tend the movement from right-top to left-bottom, the centroid of the position can be set as: y-x
-- such that the the closer to the homebase, the larger the centroid is
-- also, considering the symmetric of the board across the diagonal line, hence, the search space could be halved 

centroid :: OccupiedBoard -> Int
centroid oboard = centroidOfBoard oboard 0

centroidOfBoard :: OccupiedBoard -> Int -> Int
centroidOfBoard _ 7 = 0
centroidOfBoard oboard y = centroidOfRow oboard 0 y + centroidOfBoard oboard (y+1)

centroidOfRow :: OccupiedBoard -> Int -> Int -> Int
centroidOfRow _ 7 _ = 0
centroidOfRow oboard x y = if getElement oboard (x, y) == 1 then centroidPos (x, y) + centroidOfRow oboard (x+1) y
                           else centroidOfRow oboard (x+1) y

centroidPos :: Pos -> Int
centroidPos (x, y) = y - x

symmetric1 :: OccupiedBoard -> OccupiedBoard
symmetric1 = reverse . transpose . reverse

symmetric2 :: OccupiedBoard -> OccupiedBoard
symmetric2 = transpose

-- transform the board state into hashed value
-- hashFlipLists (flipLists test (centroid test) (dListForBoard test))
hashFlipLists :: [(OccupiedBoard, Int)] -> [(Int, Int)]
hashFlipLists = map (\ x -> (hashState (fst x) randomBoardState, snd x))

-- should avoid implementing mirror/symmetric state to reduce the search space
-- diagonal line from top right to left bottom
mirrorCheck :: [(OccupiedBoard, Int)] -> [(OccupiedBoard, Int)]
mirrorCheck [] = []
mirrorCheck (x:xs) = let (oBoard, _) = x
                         mirrorBoard = symmetric1 oBoard
                     in  if mirrorBoard `elem` map fst xs then mirrorCheck xs
                         else x:mirrorCheck xs

-- implement the list of movements and the resulting board states
flipLists :: OccupiedBoard -> Int -> [(Pos, [Pos])] -> [(OccupiedBoard, Int)]
flipLists _ _ [] = []
flipLists b c (x:xs) = let (p, ps) = x
                       in  map (flipBoardState b c p) ps ++ flipLists b c xs

-- exchange two pieces' states on the occupied board
flipBoardState :: OccupiedBoard -> Int -> Pos -> Pos -> (OccupiedBoard, Int)
flipBoardState b c (fx, fy) (tx, ty) = let newRow1 = replace fx (flip $ getElement b (fx, fy)) (b !! fy)
                                           newBoard1 = replace fy newRow1 b
                                           newRow2 = replace tx (flip $ getElement newBoard1 (tx, ty)) (newBoard1 !! ty)
                                       in  (replace ty newRow2 newBoard1, c - centroidPos (fx, fy) + centroidPos (tx, ty))
    where
        flip :: Int -> Int
        flip 0 = 1
        flip _ = 0

-- provide the available pieces and their movements
dListForBoard :: OccupiedBoard -> [(Pos, [Pos])]
dListForBoard oBoard = let ps = findPieces oBoard
                       in  zip ps (map (destinationList' oBoard) ps) -- zip the from and to destinations

-- find the occupied positions of the board/find the pieces' positions on the board
findPieces :: OccupiedBoard -> [Pos]
findPieces oBoard = [(x, y) | (y, row) <- zip [0..] oBoard, x <- elemIndices 1 row]

-- A similar movements list should be provided, but this time only position is needed
destinationList' :: OccupiedBoard -> Pos -> [Pos]
destinationList' oBoard p = nub $ findAvaliableNeighbors' oBoard p ++ searchWithoutLooping' oBoard [] p

-- stepping to an adjacent position
findAvaliableNeighbors' :: OccupiedBoard -> Pos -> [Pos]
findAvaliableNeighbors' oBoard p = filter (not . testOccupiedPos' oBoard) (findValidNeighbors' p oBoard)

findValidNeighbors' :: Pos -> OccupiedBoard -> [Pos]
findValidNeighbors' (x, y) oBoard = filter testValidPos' [(x, y-1), (x-1, y-1), (x-1, y), (x, y+1), (x+1, y+1), (x+1, y)]

testOccupiedPos' :: OccupiedBoard -> Pos -> Bool
testOccupiedPos' oBoard p = getElement oBoard p == 1

testValidPos' :: Pos -> Bool
testValidPos' (x, y) = x >= 0 && y >= 0 && x <= 6 && y <= 6

-- jumping over adjacent pieces
searchWithoutLooping' :: OccupiedBoard -> [Pos] -> Pos ->  [Pos]
searchWithoutLooping' oBoard l p = let s = jumpToAllDirections' oBoard p
                                       renewList = filter (`notElem` l) s
                                       recordList = renewList ++ l
                                   in  concatMap (searchWithoutLooping' oBoard recordList) renewList ++ renewList

jumpToAllDirections' :: OccupiedBoard -> Pos -> [Pos]
jumpToAllDirections' oBoard pos = filter (/= pos) (jumpToOneDirection' oBoard pos [(0, -1), (-1, -1), (-1, 0), (0, 1), (1, 1), (1, 0)])

jumpToOneDirection' :: OccupiedBoard -> Pos -> [Pos] -> [Pos]
jumpToOneDirection' _ _ [] = []
jumpToOneDirection' oBoard pos (a:as) = determineValidJump' oBoard pos (f a): jumpToOneDirection' oBoard pos as
    where
        f (a, b) (x, y) = (a+x, b+y)

determineValidJump' :: OccupiedBoard -> Pos -> (Pos -> Pos) -> Pos
determineValidJump' oBoard pos f
    | not (testValidPos' (f pos)) || not (testValidPos' ((f . f) pos)) = pos -- invalid ones
    | testOccupiedPos' oBoard (f pos) && not (testOccupiedPos' oBoard ((f . f) pos)) = (f . f) pos
    | otherwise = pos -- no ways found
