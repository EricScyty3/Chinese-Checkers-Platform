module Configuration where
-- produce the occupied boards in all possible states allowing BFS to compute theirs shortest moves and store them in a container
-- since computation could be considerably large for all states, only certain sections are computed
-- can be ignored, if not interested in how the dataset is arranged

import System.IO
    ( hClose,
      openFile,
      hGetContents,
      hPutStr,
      IOMode(ReadMode, WriteMode) )
import Zobrist ( hash )
import BFS
    ( centroid,
      centroidPos,
      shortestMoves,
      symmetric1_pos,
      symmetric2_pos )
import Board ( Pos )
import Control.Parallel ( par, pseq )
import Control.Monad.ST ( runST )
import Data.STRef ( modifySTRef, newSTRef, readSTRef )
import RBTree ( rbInsert, rbSearch, RBTree(..) )
import Data.Maybe ( isJust, isNothing )
import GHC.IO ( unsafePerformIO )
import System.Directory.Extra (doesFileExist)

{-
main :: IO ()
main = do arg <- getArgs
          let (tree, size) = sufficientBoards
              treeNum = read (head arg) -- the amount of program being separated 
              treeIdx = read (arg !! 1) -- which section to be computed
              width = read (arg !! 2)   -- the width for search the shortest moves, normally '(800,200)' 
              ts = splitTree tree treeNum
              items = tableElementsConstruct (ts !! treeIdx) width
          tableElementsRecord items ("../dataset/lookup_table_" ++ show treeIdx ++ ".txt")   
-}

--Database Construct-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- record and calculate the board value for each board state

-- considering processing just one huge tree could be very inefficient, an alternative is to 
-- divide the tree into several smaller sub-trees and run them in parallel, finally combine into one
splitTree :: RBTree [Pos] -> Int -> [RBTree [Pos]]
splitTree RBLeaf _ = [RBLeaf]
splitTree t 1 = [t]
splitTree (RBNode _ content t1 key t2) divfactor = let leftTree = rbInsert key content t1
                                                   in  splitTree leftTree (divfactor `div` 2) ++ splitTree t2 (divfactor `div` 2)

-- record the board state tree with its hashed as well as the corresponding minimum/shortest moves into a file
tableElementsRecord :: [(Int, Int, Int)] -> String -> IO ()
tableElementsRecord contents filePath = do filePath <- openFile filePath WriteMode
                                           hPutStr filePath (convertToString contents)
                                           hClose filePath
                                           return ()
    where
        convertToString :: [(Int, Int, Int)] -> String
        convertToString [] = ""
        -- every row will only contain 100 items
        convertToString ts = show (take 100 ts) ++ "\n" ++ convertToString (drop 100 ts) 

-- given a tree of entities, retrieve their shortest path toward the win state of a single-agent board
-- as well as the mirror images to fill the dataset
tableElementsConstruct :: RBTree [Pos] -> (Int, Int) -> [(Int, Int, Int)]
tableElementsConstruct RBLeaf _ = []
tableElementsConstruct (RBNode _ ps t1 key t2) widths@(width1, width2) = let leftsubTree  = tableElementsConstruct t1 widths
                                                                             rightsubTree = tableElementsConstruct t2 widths
                                                                         in  leftsubTree ++ [newElement] ++ rightsubTree
    where
        x = shortestMoves ps width1 -- the farther it is from the goal state, the wider breadth it might need
        y = shortestMoves (symmetric2_pos ps) width2 -- the closer it is, the less wide breadth could be sufficient 
        newElement = x `par` y `pseq` (key, x, y)

--Board Generations------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- produced the configuration of sufficient states to build a reliable lookup table for evaluation
{-
The needed configurations will be shown as following, ignoring the middle-game stage because the lookup table is quite weak for this,
hence, the size of the table could also be reduced.
Besides, according to the mirror states sharing the same shortest paths, these can be eliminated as well. 

    [0, 1, 1, 1, 1, 1, 1]
    [0, 0, 1, 1, 1, 1, 1]
    [0, 0, 0, 1, 1, 1, 1]
    [0, 0, 0, 0, 1, 1, 1]
    [0, 0, 0, 0, 0, 1, 1]
    [0, 0, 0, 0, 0, 0, 1]
    [0, 0, 0, 0, 0, 0, 0]                
-}

-- list all needed opening states to be computed and stored 
-- store all sufficient board states into a tree
sufficientBoards :: (RBTree [Pos], Int)
sufficientBoards = let ps = listAllPermutations 6 ([], 0)
                   in  boardTree ps RBLeaf 0

-- make use of the tree's fast search feature, to get rid of the mirror image of the board
boardTree :: [[Pos]] -> RBTree [Pos] -> Int -> (RBTree [Pos], Int) -- also return the total entities as part of the result
boardTree [] rb size = (rb, size)
boardTree (p:ps) rb size = let -- to avoid opening mirror images, the hash of the positions needs to be checked first
                               p2 = symmetric1_pos p
                               hash1 = hash p
                               hash2 = hash p2
                           in  -- add if not duplicated
                               if hash1 `par` hash2 `pseq` isNothing (rbSearch hash2 rb) then boardTree ps (rbInsert hash1 p rb) (size+1) 
                               else boardTree ps rb size -- otherwise, skip this one
{-
    [0, 1, 1, 1, 1, 1, 1] [0 .. 5]  0 -> (1, 0)
    [0, 0, 1, 1, 1, 1, 1] [6 ..10]  6 -> (2, 1)
    [0, 0, 0, 1, 1, 1, 1] [11..14] 11 -> (3, 2)
    [0, 0, 0, 0, 1, 1, 1] [15..17] 15 -> (4, 3)
    [0, 0, 0, 0, 0, 1, 1] [18..19] 18 -> (5, 4)
    [0, 0, 0, 0, 0, 0, 1] [20]     20 -> (6, 5)
    [0, 0, 0, 0, 0, 0, 0]
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

-- treat the occupied board as a 1D list of length 21, and return all the possible position combinations of the pieces
-- in other words, this returns a list of possible opening states' positions through permute operations
listAllPermutations :: Int -> ([Pos], Int) -> [[Pos]]
listAllPermutations 0 (ls, _) = [ls]
listAllPermutations pieces (ls, startIdx) = let idx = [startIdx .. 21 - pieces] -- settle the allowed index of element for permutation
                                                nls = map idx2Pos idx -- convert the index to 2-dimension coordinate
                                                pls = map (flipBoardState ls) nls -- concatenating the previously investigated positions
                                                next = map (+1) idx -- push forward the range as every element before this will already be manipulated 
                                            in  pls `par` next `pseq` concatMap (listAllPermutations (pieces - 1)) (zip pls next) -- expand the permutation
    where
        flipBoardState :: [Pos] -> Pos -> [Pos]
        flipBoardState ls p = p:ls

--Board Evaluation------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- evaluating a board state based on the generated lookup table

-- compute a list of board configurations, with a mixed-strategy evaluator that combines both shortest path and centroid heuristics
boardEvaluations :: [[Pos]] -> [Int]
boardEvaluations ps = -- for consistent purpose, if found a midgame states existed in the list
                      -- the evaluation will only be taken place based on the centroid heuristic
                      if ifExistMidgame ps then map centroid ps
                      -- otherwise, the lookup table is allowed to be used as reference
                      else map boardEvaluation ps
    where                      
        -- search for a shortest path for a certain board configuration based on the given dataset
        boardEvaluation :: [Pos] -> Int
        boardEvaluation ps = case evaluateBoard ps (isOpening ps) of
                                Nothing -> error ("Cannot find such board configuration: " ++ show ps)
                                Just x  -> 28 - x

        -- search for the shortest path value of a certain board configuration
        evaluateBoard :: [Pos] -> Bool -> Maybe Int
        evaluateBoard ps flag = -- if the entered board is at the opening stage
                                if flag then getFst (getShortestPath ps) -- the opening value is stored at the first position
                                -- it is necessary to flip it to the opening state first, if the enter state is an endgame
                                else getSnd (getShortestPath (symmetric2_pos ps)) -- the endgame value is stored at the second position
            
        -- load the content based on the entered board state
        getShortestPath :: [Pos] -> Maybe (Int, Int)
        getShortestPath ps = let x1 = rbSearch (hash ps) lookupTable
                                 x2 = rbSearch (hash (symmetric1_pos ps)) lookupTable -- search for the mirror opening state as well
                             in  if x1 `par` x2 `pseq` isJust x1 then x1
                                 else if isJust x2 then x2
                                      else Nothing

        getFst :: Maybe (Int, Int) -> Maybe Int
        getFst (Just (x, _)) = Just x
        getFst Nothing = Nothing

        getSnd :: Maybe (Int, Int) -> Maybe Int
        getSnd (Just (_, y)) = Just y
        getSnd Nothing = Nothing

-- determine if a board configuration is at the opening, endgame or midgame state
isOpening :: [Pos] -> Bool -- requires a symmetric operation to search in the lookup tree
isOpening = all ((<= -1) . centroidPos)
isEndgame :: [Pos] -> Bool -- requires two symmetric operation to search in the lookup tree
isEndgame = all ((>= 1) . centroidPos)
isMidgame :: [Pos] -> Bool -- cannot be found in the lookup tree, needs additional evaluation function
isMidgame ps = not (isOpening ps) && not (isEndgame ps)
-- determine if there exist midgame in a list of positions
ifExistMidgame :: [[Pos]] -> Bool
ifExistMidgame = any isMidgame

-- the structure of the container, the entities are arranged in a binary search tree style
type LookupTable = RBTree (Int, Int) 

-- the configuration dataset of the Chinese Checkers's board
lookupTable :: LookupTable
lookupTable = let elems = loadTableElements
              in  runST $ do n <- newSTRef RBLeaf
                             modifySTRef n (constructTable elems)
                             readSTRef n

-- construct the red-black tree based on the stored data with and hash of the board and 
-- the corresponding moves and symmetric endgame board's moves
constructTable :: [(Int, Int, Int)] -> LookupTable -> LookupTable
constructTable [] tree = tree
constructTable ((boardHash, opening, endgame):xs) tree = constructTable xs (rbInsert boardHash (opening, endgame) tree)

-- load the stored lookup table data from the file
loadTableElements :: [(Int, Int, Int)]
loadTableElements = let filename1 = "../dataset/lookup_table.txt"
                        filename2 = "./dataset/lookup_table.txt"
                    in  unsafePerformIO $ -- backdoor to extract the content from IO process 
                        do does1Exist <- doesFileExist filename1
                           filePath <- if does1Exist then openFile filename1 ReadMode
                                       else openFile filename2 ReadMode
                           contents <- hGetContents filePath
                           return $ convertToElement (lines contents)
    where
        convertToElement :: [String] -> [(Int, Int, Int)]
        convertToElement s = concatMap read s


{-
-- `lp 49 6 = 13983816`
lp u d = lm u (u-d+1) `div` lc d

lc 1 = 1
lc x = x * lc (x-1)

lm x m
    | x == m = m
    | otherwise = x * lm (x-1) m
-}