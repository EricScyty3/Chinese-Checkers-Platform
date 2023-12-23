module Configuration where
-- produce the occupied boards in all possible states allowing BFS to compute theirs shortest moves and store them in a container
-- since computation could be considerably large for all states, only certain sections are computed
-- can be ignored, if not interested in how the dataset is arranged

import System.IO
    ( hClose,
      openFile,
      hGetContents,
      hPutStr,
      IOMode(ReadMode, WriteMode),
      withFile, Handle )
import NZobrist ( hashBoard, updateHash )
-- import BFS
--     ( centroid,
--       centroidPos,
--       shortestMoves,
--       symmetric1_pos,
--       symmetric2_pos )
import NBoard ( Pos )
import Control.Parallel ( par, pseq )
import Control.Monad.ST ( runST )
import Data.STRef ( modifySTRef, newSTRef, readSTRef )
import Data.Maybe ( isJust, isNothing )
import GHC.IO ( unsafePerformIO )
import System.Directory.Extra (doesFileExist)
import Data.List (permutations)
import qualified Data.HashMap.Lazy as HML
import Control.Parallel.Strategies (parMap, rseq)
import Data.Time (getCurrentTime)
import Data.Time.Clock (diffUTCTime)
-- import Minimax (moveEvaluation)

-- in a lookup table, the key is the hashed board and the value is the shortest path pair
type LookupTable = HML.HashMap Int (Int, Int)

countCombinations :: Integer -> Integer -> Integer
countCombinations pieces totalSpace = let x = product [1..pieces]
                                          y = product [1..totalSpace]
                                          z = product [1..(totalSpace - pieces)]
                                      in  y `div` (x * z)


-- load the lookup table data from the file
loadTableElements :: IO ()
loadTableElements = let filename1 = "../dataset/lookup_table.txt"
                        filename2 = "./dataset/lookup_table.txt"
                    in  do does1Exist <- doesFileExist filename1
                           filePath <- if does1Exist then openFile filename1 ReadMode
                                       else openFile filename2 ReadMode
                           contents <- hGetContents filePath
                           let result = (\(a, b, c) -> (a, (b, c))) <$> concatMap read (lines contents) :: [(Int, (Int, Int))]
                               lookupTable = HML.fromList result
                           -- do something
                           print $ HML.lookup 556115780 lookupTable
                           hClose filePath

--Database Construct-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- record and calculate the board value for each board state

{-
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
-}

--Board Generations------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- produced the configuration of sufficient states to build a reliable lookup table for evaluation
{-
There are several information we can know from this represenation:
    - for 49 positions and 6 pieces, there are 13,983,816 possible board configurations
    - if we separate the board based on the diagonal, the board configuration can be classified as Opening and Endgame
    - also, every opening can be pointed to one endgame configuration by flipping the board
    - similarly, two configurations could also share the same heuristic value (shortest path) based on the symmetric feature

    [1, 0, 0, 0, 0, 0, 1]
    [0, 1, 0, 0, 0, 1, 0]
    [0, 0, 1, 0, 1, 0, 0]
    [0, 0, 0, 1, 0, 0, 0]
    [0, 0, 1, 0, 1, 0, 0]
    [0, 1, 0, 0, 0, 1, 0]
    [1, 0, 0, 0, 0, 0, 1]                
-}
-- list all needed opening states to be computed and stored 
-- store all sufficient board states into a tree
{-
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

    [0, 1, 1, 1, 1, 1, 1] [0 .. 6]  0 -> (0, 0)
    [0, 0, 1, 1, 1, 1, 1] [7 ..13]  7 -> (1, 0)
    [0, 0, 0, 1, 1, 1, 1] [14..20] 14 -> (2, 0)
    [0, 0, 0, 0, 1, 1, 1] [21..27] 21 -> (3, 0)
    [0, 0, 0, 0, 0, 1, 1] [28..34] 28 -> (4, 0)
    [0, 0, 0, 0, 0, 0, 1] [35..41] 35 -> (5, 0)
    [0, 0, 0, 0, 0, 0, 0] [42..48] 42 -> (6, 0)
-}
-- convert the 1-dimensional index to 2-dimensional coordinate, specifically for the 21-length list
idx2Pos :: Int -> Pos
idx2Pos idx = (idx `div` 7, idx `mod` 7)
    -- | 0  <= idx && idx <= 6  = (idx + 1, 0)
    -- | 7  <= idx && idx <= 10 = (idx - 5 + 1, 1)
    -- | 11 <= idx && idx <= 14 = (idx - 10 + 2, 2)
    -- | 15 <= idx && idx <= 17 = (idx - 14 + 3, 3)
    -- | 18 <= idx && idx <= 19 = (idx - 17 + 4, 4)
    -- | otherwise = (6, 5)
{-
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
-}

-- treat the occupied board as a 1D list of length 49, and return all the possible permutations
listAllPermutations :: Int -> ([Pos], Int) -> [[Pos]]
listAllPermutations 0 (ls, _) = [ls]
listAllPermutations pieces (ls, startIdx) = let idx = [startIdx .. 49 - pieces] -- settle the allowed index of element for permutation
                                                nls = map idx2Pos idx -- convert the index to 2-dimension coordinate
                                                pls = map (: ls) nls -- concatenating the previously investigated positions
                                                next = map (+1) idx -- push forward the range as every element before this will already be manipulated 
                                            in  concat $ parMap rseq (listAllPermutations (pieces - 1)) (zip pls next) -- expand the permutation
                                            

main :: IO ()
main = do -- start <- getCurrentTime
          let len = length $ listAllPermutations 6 ([], 0)
          putStrLn $ "The are " ++ show len ++ " permutations"
        --   end <- getCurrentTime
        --   let duration = realToFrac $ diffUTCTime end start
        --   putStrLn $ "Time cost: " ++ show duration


--Board Evaluation------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- evaluating a board state based on the generated lookup table
{-
-- search for a shortest path for a certain board configuration based on the given dataset
boardEvaluation :: [Pos] -> Int
boardEvaluation ps = case evaluateBoard ps (isOpening ps) of
                        Nothing -> error ("Cannot find such board configuration: " ++ show ps)
                        Just x  -> 28 - x
    where
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
-}




