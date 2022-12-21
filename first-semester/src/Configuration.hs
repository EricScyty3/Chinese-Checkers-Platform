module Configuration where
-- produce the occupied boards in all possible states allowing BFS to compute theirs mvoes and store them
-- since computation could be considerably large for all states, only several sections are considered
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
import Data.Time


-- this version of configuration tends to compute all possible board states (excluding mirror images) rather than deleting some sections
-- allBoards :: [OccupiedBoard]
-- allBoards = let p = listAllPermutations 6 (replicate 49 0, 0)
--             in  map convertToBoard p

-- convertToBoard :: [Int] -> OccupiedBoard
-- convertToBoard [] = []
-- convertToBoard xs = take 7 xs:convertToBoard (drop 7 xs)

-- ghc -main-is Configuration Configuration.hs -O2 -fllvm -outputdir dist
-- rm -rf dist/
-- rm Configuration

-- although the processing speed of BFS is difficult to be improved, 
-- the while process could be divided into several programs and run at the same time
main :: IO ()
main = do start <- getCurrentTime
          let (tree, size) = sufficientBoards
              items = multipleTreesTC (splitTree tree 8)
          tableElementsRecord items "lookup_table.txt"
          end <- getCurrentTime
          print $ diffUTCTime end start

--Datsbase Construct-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- record and calculate the board value for each board state

-- considering processing just one huge tree could be inefficient, an alternative is to divide the tree into several smaller trees and run them in parallel
-- such that the cost time could be reduced considerably
splitTree :: RBTree a -> Int -> [RBTree a]
splitTree RBLeaf _ = [RBLeaf]
splitTree t 1 = [t]
splitTree (RBNode c ps t1 key t2) len = let left  = rbInsert key ps t1
                                            right = RBTree.repaint RBTree.Black t2
                                        in  left `par` right `pseq` (splitTree left (len `div` 2) ++ splitTree right (len `div` 2))

-- record the board state tree with its hashed as well as the corresponding minimum moves into a file
tableElementsRecord :: [(Int, Int, Int)] -> String -> IO()
tableElementsRecord elements filePath = do filePath <- openFile filePath WriteMode
                                           hPutStr filePath (convertToString elements)
                                           hClose filePath
                                           return ()
    where
        convertToString :: [(Int, Int, Int)] -> String
        convertToString [] = ""
        convertToString ts = show (take 100 ts) ++ "\n" ++ convertToString (drop 100 ts) -- every line will only contain 100 items

-- process a list of trees that is split from a larger one in parallel
multipleTreesTC :: [RBTree [Pos]] -> [(Int, Int, Int)]
multipleTreesTC [] = []
multipleTreesTC (t:ts) = let n1 = take 1 $ tableElementsConstruct t
                             n2 = multipleTreesTC ts 
                         in  n1 `par` n2 `pseq` n1 ++ n2

-- calculate the corresponding shortest moves need to reach the goal state of a board state
-- as well as the minimum moves of its mirror
tableElementsConstruct :: RBTree [Pos] -> [(Int, Int, Int)]
tableElementsConstruct RBLeaf = []
tableElementsConstruct (RBNode _ p t1 key t2) = let left  = tableElementsConstruct t1
                                                    right = tableElementsConstruct t2
                                                in  left `par` right `pseq` (newElement:left ++ right)
    where
        x = shortestMoves p 800 -- the farther it is from the goal state, the wider breadth it might need
        y = shortestMoves (symmetric2_pos p) 200 -- the closer it is, the less wide breadth could be sufficient 
        newElement = x `par` y `pseq` (key, x, y)

--Board Generations------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- produced the configuration of sufficient states to build a reliable lookup table for evaluation
{-
The needed configurations will be shown as folllowing, ignoring the middle-game stage becuase the lookup table is quite weak for this,
alternative will be apply instead, hence, the size of the table could also be reduced.
Besides, according to the mirror states sharing the same shortest paths, these can be eliminated as well. 

    [0, 1, 1, 1, 1, 1, 1]
    [0, 0, 1, 1, 1, 1, 1]
    [0, 0, 0, 1, 1, 1, 1]
    [0, 0, 0, 0, 1, 1, 1]
    [0, 0, 0, 0, 0, 1, 1]
    [0, 0, 0, 0, 0, 0, 1]
    [0, 0, 0, 0, 0, 0, 0]                
-}

-- list all needed board states for certain patterns and stored as a tree 
sufficientBoards :: (RBTree [Pos], Int)
sufficientBoards = let ps = listAllPermutations 6 ([], 0) -- listAllPermutations 6 (replicate 21 0, 0)
                   in  boardTree ps RBLeaf 0

-- make use of the tree's fast search feature, to get rid of the mirror image of the board
boardTree :: [[Pos]] -> RBTree [Pos] -> Int -> (RBTree [Pos], Int)
boardTree [] rb size = (rb, size)
boardTree (p:ps) rb size = let -- avoid mirror images
                               p2 = symmetric1_pos p
                               hash1 = hash p
                               hash2 = hash p2
                           in  if isNothing $ hash1 `par` hash2 `pseq` rbSearch hash2 rb then boardTree ps (rbInsert hash1 p rb) (size+1) -- add if not duplicated
                               else boardTree ps rb size -- otherwise, skip


{-
    [0, 1, 1, 1, 1, 1, 1] [0 .. 5] 0 -> (1, 0)
    [0, 0, 1, 1, 1, 1, 1] [6 ..10] 6 -> (2, 1)
    [0, 0, 0, 1, 1, 1, 1] [11..14]11 -> (3, 2)
    [0, 0, 0, 0, 1, 1, 1] [15..17]15 -> (4, 3)
    [0, 0, 0, 0, 0, 1, 1] [18..19]18 -> (5, 4)
    [0, 0, 0, 0, 0, 0, 1] [20]    20 -> (6, 5)
-}

-- convert the 1-dimensional index to 2-dimensional coordinate, specifically for the 21-length list
idx2Pos :: Int -> Pos
idx2Pos idx
    | 0  <= idx && idx <= 5  = (idx + 1, 0)
    | 6  <= idx && idx <= 10 = (idx - 5 + 1, 1)
    | 11 <= idx && idx <= 14 = (idx - 10 + 2, 2)
    | 15 <= idx && idx <= 17 = (idx - 14 + 3, 3)
    | 18 <= idx && idx <= 19 = (idx - 17 + 4, 4)
    | otherwise = (6, 5)

{-
    -- convertToBoardWithSpace :: Int -> [Int] -> OccupiedBoard
    -- convertToBoardWithSpace 6 _ = [replicate 7 0]
    -- convertToBoardWithSpace i xs = (replicate spacer 0 ++ take (7 - spacer) xs) : convertToBoardWithSpace (i+1) (drop (7 - spacer) xs)
    --     where
    --         spacer = i + 1


    -- -- treat the occupied board as a 1D list of length 21
    -- listAllPermutations :: Int -> ([Int], Int) -> [[Int]]
    -- listAllPermutations 0 (ls, _) = [ls]
    -- listAllPermutations pieces (ls, startIdx) = let idx = [startIdx .. length ls - pieces] -- settle the allowed index of element for permutation
    --                                                 nls = map (flipBoardState ls) idx -- flip 0 to 1 as indicating a piece is placed
    --                                                 next = map (+1) idx -- backward the range as every element before this will already be manipulated 
    --                                             in  nls `par` next `pseq` concatMap (listAllPermutations (pieces - 1)) (zip nls next)
    --     where
    --         flipBoardState :: [Int] -> Int -> [Int]
    --         flipBoardState ls p = replace p 1 ls
-}

-- treat the occupied board as a 1D list of length 21, and return all the possible position combinations of the pieces
listAllPermutations :: Int -> ([Pos], Int) -> [[Pos]]
listAllPermutations 0 (ls, _) = [ls]
listAllPermutations pieces (ls, startIdx) = let idx = [startIdx .. 21 - pieces] -- settle the allowed index of element for permutation
                                                nls = map idx2Pos idx -- convert the index to 2-dimension coordinate
                                                pls = map (flipBoardState ls) nls -- concating the previously investigated positions
                                                next = map (+1) idx -- push forward the range as every element before this will already be manipulated 
                                            in  nls `par` next `pseq` concatMap (listAllPermutations (pieces - 1)) (zip pls next) -- expand the permutation
    where
        flipBoardState :: [Pos] -> Pos -> [Pos]
        flipBoardState ls p = p:ls


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
