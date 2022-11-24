module AStar where

import Data.Maybe
import Data.List
import ShortestPath
import Zobrist
import Board

-- try A* search
-- the priority is calculated by adding the cost (moves needed to current state) and the predicted cost (the difference between current and end centroid)
-- shortestMoves_A b = aSearch [(b, 0)] []

shortestMovesA :: OccupiedBoard -> Int
shortestMovesA b = aSearchS [(b, 0)] []

-- testLoop o c 17 = (o, c)
-- testLoop o c i = let (o1, c1) = aSearchS o c
--                  in  testLoop o1 c1 (i+1)

aSearchS :: [(OccupiedBoard, Int)] -> [(OccupiedBoard, Int)] -> Int
aSearchS openSet closeSet = let idx = hp openSet
                                (b, i) = openSet !! idx
                            in  if centroid b == 28 then i -- if reaches the goal state
                               -- if not then update then move the node from openSet to closeSet
                               -- and expand the node
                                else
                                     let newOpen = removeElement idx openSet -- remove the checked node from open set to close set
                                         newClose = (b, i):closeSet
                                         ps = dListForBoard b -- retrieve moveable positions
                                         nb = mirrorCheck $ flipListsA b i ps -- generate the resulting board states
                                         fb = filterList nb newClose -- check if the expanded nodes are already visited
                                     in  aSearchS {-(updateListA fb newOpen)-} (fb ++ newOpen) newClose
    where
        hp xs = let sl = map priority xs
                in  fromMaybe 0 (elemIndex (minimum sl) sl)

updateListA :: [(OccupiedBoard, Int)] -> [(OccupiedBoard, Int)] -> [(OccupiedBoard, Int)]
updateListA [] os = os
updateListA (n:ns) os
  | n `elem` os = updateListA ns os
  | length os < 50 = updateListA ns (n:os)
  | otherwise = let maxScore = maximum (map priority os)
                    score = priority n
                in  if score < maxScore then let idx = fromMaybe 0 (elemIndex maxScore (map priority os))
                                             in  updateListA ns (replace idx n os)
                    else updateListA ns os

filterList :: [(OccupiedBoard, Int)] -> [(OccupiedBoard, Int)] -> [(OccupiedBoard, Int)]
filterList [] _ = []
filterList (o:os) cs = if o `notElem` cs then o:filterList os cs
                       else filterList os cs

priority :: (OccupiedBoard, Int) -> Double
priority (b, i) = heuristicDist b + fromIntegral i

heuristicDist :: OccupiedBoard -> Double
heuristicDist b = let ps = getAvaliablePieces b
                      dl = concatMap (distList b) ps
                  in  if null dl then 0 else sum dl / fromIntegral (length dl) -- minimum dl


distList :: OccupiedBoard -> Pos -> [Double]
distList b x = map (dist x) (getNonOccupiedPose b homeBase)

dist :: Pos -> Pos -> Double
dist (x, y) (x1, y1) = sqrt (fromIntegral ((x-x1)^2 + (y - y1)^2))

-- this should be the non-occupied home base positions
getNonOccupiedPose :: OccupiedBoard -> [Pos] -> [Pos]
getNonOccupiedPose _ [] = []
getNonOccupiedPose b (p:ps) = if getElement b p == 0 then p:getNonOccupiedPose b ps
                              else getNonOccupiedPose b ps

homeBase :: [Pos]
homeBase = [(2,6),(1,6),(0,6),(1,5),(0,5),(0,4)]

getAvaliablePieces :: OccupiedBoard -> [Pos]
getAvaliablePieces board = getBoardPieces board 0 []
    where
        getBoardPieces :: OccupiedBoard -> Int -> [Pos] -> [Pos]
        getBoardPieces [] _ ps = ps
        getBoardPieces (r:rs) y ps = let nps = getRowPiecePos r 0 y ps
                                     in  getBoardPieces rs (y+1) nps

        getRowPiecePos :: [Int] -> Int -> Int -> [Pos] -> [Pos]
        getRowPiecePos [] _ _ ps = ps
        getRowPiecePos (r:rs) x y ps = if r == 1  && (x, y) `notElem` homeBase then getRowPiecePos rs (x+1) y ((x,y):ps)
                                       else getRowPiecePos rs (x+1) y ps

removeElement :: Int -> [a] -> [a]
removeElement idx xs = take idx xs ++ drop (idx+1) xs

-- implement the list of movements and the resulting levels
flipListsA :: OccupiedBoard -> Int -> [(Pos, [Pos])] -> [(OccupiedBoard, Int)]
flipListsA _ _ [] = []
flipListsA b i (x:xs) = let (p, ps) = x
                        in  map (flipBoardStateA b i p) ps ++ flipListsA b i xs

-- exchange two pieces' states on the occupied board
flipBoardStateA :: OccupiedBoard -> Int -> Pos -> Pos -> (OccupiedBoard, Int)
flipBoardStateA b i (fx, fy) (tx, ty) = let newRow1 = replace fx (flip $ getElement b (fx, fy)) (b !! fy)
                                            newBoard1 = replace fy newRow1 b
                                            newRow2 = replace tx (flip $ getElement newBoard1 (tx, ty)) (newBoard1 !! ty)
                                        in  (replace ty newRow2 newBoard1, i+1)
    where
        flip :: Int -> Int
        flip 0 = 1
        flip _ = 0
