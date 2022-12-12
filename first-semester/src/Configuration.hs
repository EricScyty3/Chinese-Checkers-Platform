module Configuration where

import Data.List
import System.IO
import Zobrist
import BFS
import Board
import Control.Monad.State
import Control.Monad.Extra
import Control.Parallel
import Control.Monad.ST
import Data.STRef
import RBTree
import Data.Maybe


-- this version of configuration tends to compute all possible board states (excluding mirror images) rather than deleting some sections
-- allBoards :: [OccupiedBoard]
-- allBoards = let p = listAllPermutations 6 (replicate 49 0, 0)
--             in  map convertToBoard p

-- convertToBoard :: [Int] -> OccupiedBoard
-- convertToBoard [] = []
-- convertToBoard xs = take 7 xs:convertToBoard (drop 7 xs)

sufficientBoards :: (RBTree OccupiedBoard, Int)
sufficientBoards = let p = listAllPermutations 6 (replicate 21 0, 0)
                       ps = map (convertToBoardWithSpace 0) p
                   in  boardTree ps RBLeaf 0

boardTree :: [OccupiedBoard] -> RBTree OccupiedBoard -> Int -> (RBTree OccupiedBoard, Int)
boardTree [] rb size = (rb, size)
boardTree (b:bs) rb size = let ones1 = findOccupiedPieces b -- avoid mirror images
                               ones2 = findOccupiedPieces (symmetric1 b)
                               hash1 = hash ones1
                               hash2 = hash ones2
                           in  if isNothing $ hash1 `par` hash2 `pseq` rbSearch hash2 rb then boardTree bs (rbInsert hash1 b rb) (size+1)
                               else boardTree bs rb size

convertToBoardWithSpace :: Int -> [Int] -> OccupiedBoard
convertToBoardWithSpace 6 _ = [replicate 7 0]
convertToBoardWithSpace i xs = (replicate spacer 0 ++ take (7 - spacer) xs) : convertToBoardWithSpace (i+1) (drop (7 - spacer) xs)
    where
        spacer = i + 1

listAllPermutations :: Int -> ([Int], Int) -> [[Int]]
listAllPermutations 0 (ls, _) = [ls]
listAllPermutations pieces (ls, startIdx) = let idx = [startIdx .. length ls - pieces]
                                                nls = map (flipBoardState ls) idx
                                                next = map (+1) idx
                                            in  nls `par` next `pseq` concatMap (listAllPermutations (pieces - 1)) (zip nls next)
    where
        flipBoardState :: [Int] -> Int -> [Int]
        flipBoardState ls p = replace p 1 ls
                                    
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

tableElementsConstruct :: RBTree OccupiedBoard -> [(Int, Int, Int)]
tableElementsConstruct RBLeaf = []
tableElementsConstruct (RBNode _ b t1 key t2) = let left  = tableElementsConstruct t1 
                                                    right = tableElementsConstruct t2 
                                                in  left `par` right `pseq` (newElement:left ++ right)
    where
        x = shortestMoves b 800
        y = shortestMoves (symmetric2 b) 200
        newElement = x `par` y `pseq` (key, x, y)


-- record the board state into hashed state as well as the corresponding minimum moves
tableElementsRecord :: RBTree OccupiedBoard  -> String -> IO()
tableElementsRecord boardTree filePath = do filePath <- openFile filePath WriteMode
                                            let tableElements = tableElementsConstruct boardTree
                                            hPutStr filePath (convertToString tableElements)
                                            hClose filePath
                                            return ()
    where
        convertToString :: [(Int, Int, Int)] -> String
        convertToString [] = ""
        convertToString ts = show (take 100 ts) ++ "\n" ++ convertToString (drop 100 ts)

-- ghc -main-is Configuration Configuration.hs -O2 -fllvm -outputdir dist
-- rm -rf dist/
-- rm Configuration

-- although the processing speed could not be improved, it coulod be divided into several programs and run at the same time
main :: IO ()
main = do let (RBNode c b t1 key t2, size) = sufficientBoards
              newLeftTree = rbInsert key b t1
          n1 <- tableElementsRecord newLeftTree "lookup_table1.txt"
          n2 <- tableElementsRecord t2 "lookup_table2.txt"
          n1 `par` n2 `pseq` print "Done"


                         

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
