
module Zobrist where
import System.Random
import Data.List
import Board

type StateTable = [[Int]]
type OccupiedBoard = [[Int]]

-- board state matrix: each position has one state: Occupied with its own value
randomBoardState :: StateTable
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

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- the internal single-agent board for a player with the top-right the starting point and botton-left the destination
-- this is just for heuristic board evaluation 
-- only need to know the occupy status for each position
-- In addition, the transformation between the external (display-purposed) board the internal (heuristi-purposed) board is needed
-- furthermore, in experimental environment, the communication between each player should be done through internal board exchanging
initialState :: OccupiedBoard
initialState = [
                [0, 0, 0, 0, 1, 1, 1],
                [0, 0, 0, 0, 0, 1, 1],
                [0, 0, 0, 0, 0, 0, 1],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0]]

-- a win for a player can only be achieved at one's turn
winStateDetect :: Int -> Bool
winStateDetect rh = hashEnd == rh

-- the hashed values for two states
hashInitial :: Int
hashInitial = hashState initialState randomBoardState
hashEnd :: Int
hashEnd = hashState (transpose initialState) randomBoardState

-- after the first construct, each change only need to add the changed hashes
hashChange :: (Int, Int) -> (Int, Int) -> Int -> Int
hashChange (fx, fy) (tx, ty) xv = foldr myXOR 0 [xv, f, t]
    where
        f = (bs !! fy) !! fx
        t = (bs !! ty) !! tx
        bs = randomBoardState

-- construct the hashed board state of the given occupied board state
hashState :: OccupiedBoard -> StateTable -> Int
hashState _ [] = 0
hashState [] _ = 0
hashState (x:xs) (s:ss) = myXOR (hashOneRow x s) (hashState xs ss)
    where
        hashOneRow :: [Int] -> [Int] -> Int
        hashOneRow [] _ = 0
        hashOneRow _ [] = 0
        hashOneRow (x:xs) (s:ss)
            | x == 1 = myXOR s (hashOneRow xs ss) -- only XOR the occupied position's state value
            | otherwise = hashOneRow xs ss

-- transform a decimal integer into binary list
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

-- first transform the integers into binary list and xor, and finally return as decimal integer
myXOR :: Int -> Int -> Int
myXOR x y = toDecimal $ listXOR fx fy
    where
        (fx, fy) = fixLength (toBinary x) (toBinary y)

-- xor the two binary values in lists
listXOR :: [Int] -> [Int] -> [Int]
listXOR _ [] = []
listXOR [] _ = []
listXOR (x:xs) (y:ys) = binaryXOR x y:listXOR xs ys

-- maintain the same length of the two binary values
fixLength :: [Int] -> [Int] -> ([Int], [Int])
fixLength x y
    | lx > ly = (x, fillZeros y (lx - ly))
    | lx < ly = (fillZeros x (ly - lx), y)
    | otherwise = (x, y)
    where
        lx = length x
        ly = length y

-- extend the length with 0s at the front
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