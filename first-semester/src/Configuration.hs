module Configuration where

import Data.List
import Zobrist
import Board
import ShortestPath

-- list the all configurations of the board states, could be large but also possible to decrease the size
validBoards :: [OccupiedBoard]
validBoards = let bs = listAllBoards (listAllRows 7) 7
              in  filter ((==6) . mySum) bs

mySum :: [[Int]] -> Int
mySum rs = sum (map sum rs)

doubleMirrorCheck :: [OccupiedBoard] -> [OccupiedBoard] -> [OccupiedBoard]
doubleMirrorCheck [] as = as
doubleMirrorCheck (b:bs) as = if symmetric1 b `notElem` as && symmetric2 b `notElem` as then doubleMirrorCheck bs (b:as)
                              else doubleMirrorCheck bs as

listAllBoards :: [[Int]] -> Int -> [OccupiedBoard]
listAllBoards _  0 = [[]]
listAllBoards rs l = [x:xs | x <- rs, xs <- listAllBoards rs (l - 1)]

listAllRows :: Int -> [[Int]]
listAllRows 0 = [[]]
listAllRows l = [x:xs | x <- [0..1], xs <- listAllRows (l - 1)]


-- `lp 49 6 = 13983816`
-- lp u d = lm u (u-d+1) `div` lc d

-- lc 1 = 1
-- lc x = x * lc (x-1)

-- lm x m
--     | x == m = m
--     | otherwise = x * lm (x-1) m