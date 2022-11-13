module Zobrist where

import System.Random
import Data.List

type StateTable = [[(Int, Int)]]
-- type OccupiedBoard = [[Int]]

-- board state matrix: each position has two states, Occupied and Avaliable, and each state has its own value
printBoard :: Show a => [a] -> IO ()
printBoard [] = putStr ""
printBoard (x:xs) = do print x
                       printBoard xs

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

testBoard :: [[Int]]
testBoard = [
    [0,0,0,0,1,1,1],
    [0,0,0,0,0,1,1],
    [0,0,0,0,0,0,1],
    [0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0]]

hashInitial :: Int
hashInitial = hashState testBoard randomBoardState

-- after the first construct, each change only need to add the changed hashes
hashChange :: (Int, Int) -> (Int, Int) -> StateTable -> Int -> Int
hashChange (fx, fy) (tx, ty) bs xv = foldr myXOR 0 [xv, fo, fa, to, ta]
    where
        (fo, fa) = (bs !! fy) !! fx
        (to, ta) = (bs !! ty) !! tx

-- construct the hashed board state of the given occupied board state
hashState :: [[Int]] -> StateTable -> Int
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