module Zobrist where

import System.Random
import Data.List
import Board

type StateTable = [[(Int, Int)]]
type OccupiedBoard = [[Int]]

-- board state matrix: each position has two states, Occupied and Avaliable, and each state has its own value
{-
    [(49,152),(96,16),(209,211),(146,24),(140,245),(51,188),(134,143)]
    [(42,249),(95,93),(92,238),(128,67),(10,76),(156,123),(25,225)]
    [(75,163),(227,236),(32,169),(220,35),(190,73),(108,241),(126,151)]
    [(185,120),(160,246),(39,4),(98,87),(81,179),(78,66),(204,167)]
    [(59,132),(251,136),(133,106),(30,237),(14,159),(221,28),(184,186)]
    [(19,193),(196,201),(45,29),(99,70),(116,147),(41,84),(218,174)]
    [(240,137),(83,208),(63,207),(46,107),(242,248),(47,130),(52,64)]
-}
randomBoardState :: [[(Int, Int)]]
randomBoardState = randomBoardColumn randomList
    where
        randomBoardColumn :: [Int] -> [[(Int, Int)]]
        randomBoardColumn [] = []
        randomBoardColumn xs = randomRowState (take 14 xs) : randomBoardColumn (drop 14 xs)

        randomRowState :: [Int] -> [(Int, Int)]
        randomRowState [] = [] 
        randomRowState [_] = []
        randomRowState (x:y:ss) = (x, y):randomRowState ss

        -- static random list without duplicate values
        randomList :: [Int]
        randomList = take 98 $ nub $ randomRs (1, 256) (mkStdGen 42)

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

{-
-- exchange two pieces' states on the occupied board
flipBoardState :: (Int, Int) -> (Int, Int) -> OccupiedBoard -> OccupiedBoard
flipBoardState (fx, fy) (tx, ty) b = let newRow1 = replace fx (flip $ getElem b (fx, fy)) (b !! fy)
                                         newBoard1 = replace fy newRow1 b
                                         newRow2 = replace tx (flip $ getElem newBoard1 (tx, ty)) (newBoard1 !! ty)
                                     in  replace ty newRow2 newBoard1
    where
        flip :: Int -> Int
        flip 0 = 1
        flip _ = 0
        getElem :: OccupiedBoard -> (Int, Int) -> Int
        getElem b (x, y) = (b !! y) !! x   
-}
-- the hashed values for two states
hashInitial :: Int
hashInitial = hashState initialState randomBoardState
hashEnd :: Int
hashEnd = hashState (transpose initialState) randomBoardState

-- after the first construct, each change only need to add the changed hashes
hashChange :: (Int, Int) -> (Int, Int) -> Int -> Int
hashChange (fx, fy) (tx, ty) xv = foldr myXOR 0 [xv, fo, fa, to, ta]
    where
        (fo, fa) = (bs !! fy) !! fx
        (to, ta) = (bs !! ty) !! tx
        bs = randomBoardState

-- construct the hashed board state of the given occupied board state
hashState :: OccupiedBoard -> StateTable -> Int
hashState _ [] = 0
hashState [] _ = 0
hashState (x:xs) (s:ss) = myXOR (hashOneRow x s) (hashState xs ss)
    where
        hashOneRow :: [Int] -> [(Int, Int)] -> Int
        hashOneRow [] _ = 0
        hashOneRow _ [] = 0
        hashOneRow (x:xs) (s:ss)
            | x == 1 = myXOR o (hashOneRow xs ss)
            | otherwise = myXOR a (hashOneRow xs ss)
            where
                (o, a) = s

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