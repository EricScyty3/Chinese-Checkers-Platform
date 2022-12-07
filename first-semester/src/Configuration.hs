module Configuration where

import Data.List
import System.IO
import Zobrist
import ShortestPath
import Board
import Control.Monad.State
import Control.Parallel

-- result the sufficient amount of board states with identical effect
sufficientBoards :: [OccupiedBoard]
sufficientBoards = mirrorBoardCheck (validBoards $ listBoards $ listRows 0 7)

-- ensure the boards that are unique, can reduce a factor close to 2
mirrorBoardCheck :: [OccupiedBoard] -> [OccupiedBoard]
mirrorBoardCheck [] = []
mirrorBoardCheck (b:bs) = if symmetric1 b `notElem` bs then b:mirrorBoardCheck bs
                          else mirrorBoardCheck bs

-- omit the boards with less than 6 pieces
validBoards :: [OccupiedBoard] -> [OccupiedBoard]
validBoards = filter ((== totalPieces) . piecesCount)
  where
    piecesCount :: [[Int]] -> Int
    piecesCount rs = sum (map sum rs)

-- arrange the board state with different row states
listBoards :: [[[Int]]] -> [OccupiedBoard]
listBoards [] = [[]]
listBoards (r:rs) = [x:xs | x <- r, xs <- listBoards rs]

-- compute different rows with different pieces allowed
listRows :: Int -> Int -> [[[Int]]]
listRows i w
    | i /= w = map (replicate (i+1) 0 ++ ) (permutation (w - (i+1))):listRows (i+1) w
    | otherwise = []

-- arrange a binary list with certain length
permutation :: Int -> [[Int]]
permutation 0 = [[]]
permutation l = [x:xs | x <- [0..1], xs <- permutation (l - 1)]

{-
The needed configurations will be shown as folllowing, ignoring the middle-game stage becuase the lookup table is quite weak for this,
alternative will be apply instead, hence, the size of the table could also be reduced
Besides, according to the mirror states sharing the same shortest paths, these can be eliminated

    [0, 1, 1, 1, 1, 1, 1]
    [0, 0, 1, 1, 1, 1, 1]
    [0, 0, 0, 1, 1, 1, 1]
    [0, 0, 0, 0, 1, 1, 1]
    [0, 0, 0, 0, 0, 1, 1]
    [0, 0, 0, 0, 0, 0, 1]
    [0, 0, 0, 0, 0, 0, 0]                
-}

-- transform the board state into hashed value and calculate the corresponding shortest moves need to reach the goal state, 
-- as well as the minimum moves of its mirror
tableElementsConstruct :: [OccupiedBoard] -> [(Int, Int, Int)]
tableElementsConstruct [] = []
tableElementsConstruct (b:bs) = newElement:tableElementsConstruct bs
    where
        x = shortestMoves b 200
        y = shortestMoves (symmetric2 b) 200
        newElement = x `par` y `pseq` (hashState b, x, y)
        hashState board = evalState (hashBoardWithPos (findPieces board)) randomBoardState

-- record the board state into hashed state as well as the corresponding minimum moves
tableElementsRecord :: [OccupiedBoard] -> IO()
tableElementsRecord bs = do filePath <- openFile "lookup_table.txt" WriteMode
                            let tableElements = tableElementsConstruct bs
                            hPutStr filePath (convertToString tableElements)
                            hClose filePath
    where
        convertToString :: [(Int, Int, Int)] -> String
        convertToString [] = ""
        convertToString ts = show (take 100 ts) ++ "\n" ++ convertToString (drop 100 ts)
-- ghc Configuration.hs -O2 -fllvm -outputdir dist
-- rm -rf dist/
-- rm Configuration
main = tableElementsRecord (take 200 sufficientBoards)

-- -- load the stored lookup table data from the file
-- loadTableElements :: [(Hash, StoredData)]
-- {-# NOINLINE loadTableElements #-}
-- loadTableElements = unsafePerformIO loadRecord
--     where
--         loadRecord :: IO [(Hash, StoredData)]
--         loadRecord = do filePath <- openFile "lookup_table.txt" ReadMode
--                         contents <- hGetContents filePath
--                         -- hClose filePath
--                         return $ convertToElement (lines contents)

--         convertToElement :: [String] -> [(Hash, StoredData)]
--         convertToElement s = map (\(x, y, z) -> (x, [y, z])) (concatMap read s)

-- buildLookupTable :: RBTree
-- buildLookupTable = createTree loadTableElements

-- `lp 49 6 = 13983816`
lp u d = lm u (u-d+1) `div` lc d

lc 1 = 1
lc x = x * lc (x-1)

lm x m
    | x == m = m
    | otherwise = x * lm (x-1) m
