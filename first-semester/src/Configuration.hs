module Configuration where

import Data.List
import System.IO
import Zobrist
import ShortestPath
import Board
import Control.Monad.State
import Control.Monad.Extra
import Control.Parallel
import Control.Monad.ST
import Data.STRef
import RBTree (RBTree, rbSearch, rbInsert, emptyTree)
-- import qualified Data.Map as Map


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
                   in  evalState (myTest ps emptyTree 0) randomBoardState

myTest :: [OccupiedBoard] -> RBTree OccupiedBoard -> Int -> State StateTable (RBTree OccupiedBoard, Int)
myTest [] rb c = return (rb, c)
myTest (b:bs) rb c = do let ones = findPieces b -- avoid mirror images
                            ones2 = findPieces (symmetric1 b)
                        hash  <- hashBoardWithPos ones
                        hash2 <- hashBoardWithPos ones2
                        if hash `par` hash2 `pseq` rbSearch hash2 rb then myTest bs rb c
                        else myTest bs (rbInsert hash b rb) (c+1)

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
flipBoardState :: [Int] -> Int -> [Int]
flipBoardState board p = runST $ do n <- newSTRef board
                                    modifySTRef n (replace p 1)
                                    readSTRef n

-- -- result the sufficient amount of board states with identical effect
-- sufficientBoards :: [OccupiedBoard]
-- sufficientBoards = mirrorBoardCheck (validBoards $ listBoards $ listRows 0 7)


-- -- ensure the boards that are unique, can reduce a factor close to 2
-- mirrorBoardCheck :: [OccupiedBoard] -> [OccupiedBoard]
-- mirrorBoardCheck [] = []
-- mirrorBoardCheck (b:bs) = if symmetric1 b `notElem` bs then b:mirrorBoardCheck bs
--                           else mirrorBoardCheck bs

-- -- omit the boards with less than 6 pieces
-- validBoards :: [OccupiedBoard] -> [OccupiedBoard]
-- validBoards = filter ((== totalPieces) . piecesCount)
--   where
--     piecesCount :: [[Int]] -> Int
--     piecesCount rs = sum (map sum rs)

-- -- arrange the board state with different row states
-- listBoards :: [[[Int]]] -> [OccupiedBoard]
-- listBoards [] = [[]]
-- listBoards (r:rs) = [x:xs | x <- r, xs <- listBoards rs]

-- -- compute different rows with different pieces allowed
-- listRows :: Int -> Int -> [[[Int]]]
-- listRows i w
--     | i /= w = map (replicate (i+1) 0 ++ ) (replicateM (w - (i+1)) [0, 1]):listRows (i+1) w
--     | otherwise = []

-- -- arrange a binary list with certain length
-- permutation :: Int -> [[Int]]
-- permutation 0 = [[]]
-- permutation l = [x:xs | x <- [0..1], xs <- permutation (l - 1)]

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
{-
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
-}
-- ghc Configuration.hs -O2 -fllvm -outputdir dist
-- rm -rf dist/
-- rm Configuration
main = do let (_, size) = sufficientBoards
          print size

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
