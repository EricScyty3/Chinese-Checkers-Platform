-- the operator of hashing, could be use to represent a certain state sufficiently 
module Zobrist where

import Data.List
import Board
import Control.Monad.State
import Control.Monad.Extra
import Control.Parallel
import System.Random
import Data.STRef
import Control.Monad.ST


-- a list of unique random integers 
type StateTable = [[Int]]

-- board state table: each position can only have one state, either occupied or not, and if a position is occupied, the stored value will be applied
{- 
randomBoardState = randomBoardColumn randomList 0
    where
        -- construct the matrix 
        randomBoardColumn :: [Int] -> Int -> StateTable
        randomBoardColumn _  7 = []
        randomBoardColumn [] _ = []
        randomBoardColumn xs i = take 7 xs : randomBoardColumn (drop 7 xs) (i+1)
        
        -- static random list without duplicate values
        randomList :: [Int]
        randomList = nub $ randomRs (1, 2^32) (mkStdGen 42)
-}

randomBoardState :: StateTable
randomBoardState = [
                    [3292324401, 233489048,  2624610400, 1597242128, 1980104913, 1321694675, 3441975186],
                    [1130700312, 1305326220, 2205018208, 2326285813, 2296381747, 3769793212, 2531570566],
                    [3207055759, 1137426218, 2956685049, 4256428639, 724082013,  2138168924, 2728019182],
                    [2087020672, 2189657155, 903285258,  1992067404, 2726019740, 1298665595, 1408913945],
                    [2990988946, 3063264481, 149517643,  1100318883, 2752873187, 3781215980, 2792287776],
                    [977698729,  118633436,  2784537123, 1886397907, 1695135422, 92683337,   2971222636],
                    [1857154033, 3253362046, 1756536471, 2064999353, 510226296,  402957728,  3185258486]]

--Hash Operators---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- the internal single-agent board for a player with the top-right the starting point and botton-left the destination
-- this is just for heuristic board evaluation, therefore, only need to know the occupy status for each position

-- the empty state
{-
empty :: OccupiedBoard
empty = replicate occupiedBoardSize (replicate occupiedBoardSize 0)
-- the initial state
initialState :: OccupiedBoard
initialState = [
                [0, 0, 0, 0, 1, 1, 1],
                [0, 0, 0, 0, 0, 1, 1],
                [0, 0, 0, 0, 0, 0, 1],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0]]

-- find the occupied positions of the board/find the pieces' positions on the occupied board
findOccupiedPieces :: OccupiedBoard -> [Pos]
findOccupiedPieces board = sort $ [(x, y) | (y, row) <- zip [0..] board, x <- elemIndices 1 row]
-- project the positions of the pieces on the exnterl board into internal board
-- instead of a full table, a list of occupied positions could be more efficient to represent the internal board
convertToInternalBoard :: Board -> Colour -> [Pos]
convertToInternalBoard eboard colour = map (projection colour . getPos) (findPiecesWithColour colour eboard)
-}

-- the hashed values of the both initial and win states
hashInitial :: Int
hashInitial = hash startBase
hashEnd :: Int
hashEnd = hash goalBase

-- a win for a player can be detected based on comparing the hashed values
winStateDetect :: [Pos] -> Bool
winStateDetect ps = hashEnd == hash ps

-- given a list of positions on the pieces on the current occupied board and return a hashed value
hash :: [Pos] -> Int
hash ps = let posPL = filter (testValidPos occupiedBoardSize occupiedBoardSize) ps -- filter the negative positions
          in  evalState (hashBoardWithPos posPL) randomBoardState
    where
        hashBoardWithPos :: [Pos] -> State StateTable Int
        hashBoardWithPos [] = return 0
        hashBoardWithPos (p:ps) = do randomState1 <- getElement p
                                     randomState2 <- hashBoardWithPos ps
                                     return $ randomState1 `par` randomState2 `pseq` (randomState1 `myXOR` randomState2)

-- if the board state is change, just hash the changed positions with the previous value, and a new hash will be provided
-- changeHash :: Pos -> Pos -> Int -> Int
-- changeHash x y h = evalState (hashChange x y h) randomBoardState
--     where
--         -- after the first construct, each changed hash does not need to recalculate, just applys the two changed positions' hashes
--         hashChange :: Pos -> Pos -> Int -> State StateTable Int
--         hashChange fp tp xv = do f <- safeGetElement fp -- if the position is negative, then treat it as 0
--                                  t <- safeGetElement tp
--                                  return $ f `par` t `pseq` foldr myXOR 0 [xv, f, t]

--         safeGetElement :: Pos -> State StateTable Int
--         safeGetElement pos = do if not $ testValidPos occupiedBoardSize occupiedBoardSize pos then return 0
--                                 else getElement pos

-- given a positions changes, generate a list of resulting boards
-- flipBoards ::  [Pos] -> [(Pos, Pos)] -> [[Pos]]
-- flipBoards _ [] = []
-- flipBoards ps (x:xs) = flipBoard ps x:flipBoards ps xs
-- given a positions change, reflect that onto the internal board
flipBoard :: [Pos] -> (Pos, Pos) -> [Pos]
flipBoard ps (x, y) = case x `elemIndex` ps of
                        Nothing -> error "Not existing such position in the list"
                        Just idx -> replace idx y ps

--Hash Construct---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- transform a decimal integer into a binary list
toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = reverse (toBinary' n)
    where
        toBinary' :: Int -> [Int]
        toBinary' 0 = []
        toBinary' n
            | even n = 0:toBinary' (n `div` 2)
            | otherwise = 1:toBinary' (n `div` 2)

-- transform a binary list into a decimal integer 
toDecimal :: [Int] -> Int
toDecimal [] = 0
toDecimal (x:xs) = (x * 2 ^ length xs) + toDecimal xs

-- first transform the integers into binary list and then apply xor operator, and finally return it as a decimal integer
myXOR :: Int -> Int -> Int
myXOR x y = toDecimal $ listXOR fx fy
    where
        (fx, fy) = let bx = toBinary x
                       by = toBinary y
                   in  bx `par` by `pseq` fixLength bx by

-- xor the two binary values in lists
listXOR :: [Int] -> [Int] -> [Int]
listXOR _ [] = []
listXOR [] _ = []
listXOR (x:xs) (y:ys) = let xor  = binaryXOR x y
                            xors = listXOR xs ys
                        in  xor `par` xors `pseq` xor:xors

-- maintain the same length of the two binary lists
fixLength :: [Int] -> [Int] -> ([Int], [Int])
fixLength x y
    | lx > ly = (x, fillZeros y (lx - ly))
    | lx < ly = (fillZeros x (ly - lx), y)
    | otherwise = (x, y)
    where
        lx = length x
        ly = length y

-- extend the length with 0 at the front
fillZeros :: [Int] -> Int -> [Int]
fillZeros xs l = replicate l 0 ++ xs

-- binary xor operator
binaryXOR :: Int -> Int -> Int
binaryXOR x y
    | boolXOR (x == 1) (y == 1) = 1
    | otherwise = 0

-- boolean xor operator
boolXOR :: Bool -> Bool -> Bool
boolXOR True True = False
boolXOR True False = True
boolXOR False True = True
boolXOR False False = False
