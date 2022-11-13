module Zobrist where

type StateTable = [[(Int, Int)]]
-- board state matrix: each position has two states, occupied and avaliable, and each state has its own value
boardState :: StateTable
boardState = [
    [(1, 2), (3, 4), (5, 6), (7, 8), (9, 10),(11,12),(13,14)],
    [(15,16),(17,18),(19,20),(21,22),(23,24),(25,26),(27,28)],
    [(29,30),(31,32),(33,34),(35,36),(37,38),(39,40),(41,42)],
    [(43,44),(45,46),(47,48),(49,50),(51,52),(53,54),(55,56)],
    [(57,58),(59,60),(61,62),(63,64),(65,66),(67,68),(69,70)],
    [(71,72),(73,74),(75,76),(77,78),(79,80),(81,82),(83,84)],
    [(85,86),(87,88),(89,90),(91,92),(93,94),(95,96),(97,98)]]

testBoard :: [[Int]]
testBoard = [
    [0,0,0,0,1,1,1],
    [0,0,0,0,0,1,1],
    [0,0,0,0,0,0,1],
    [0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0]]

hashChange :: (Int, Int) -> (Int, Int) -> StateTable -> Int -> Int
hashChange (fx, fy) (tx, ty) bs xv = foldr myXOR 0 [xv, fo, fa, to, ta]
    where
        (fo, fa) = (bs !! fy) !! fx
        (to, ta) = (bs !! ty) !! tx

hashInital :: [[Int]] -> StateTable -> Int
hashInital _ [] = 0
hashInital [] _ = 0
hashInital (x:xs) (s:ss) = myXOR (hashOneRow x s) (hashInital xs ss)

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
toDecimal (x:xs) = (x * 2 ^length xs) + toDecimal xs

myXOR :: Int -> Int -> Int
myXOR x y = toDecimal $ listXOR fx fy
    where
        (fx, fy) = fixLength (toBinary x) (toBinary y)

-- the two lists should be of the same length
listXOR :: [Int] -> [Int] -> [Int]
listXOR _ [] = []
listXOR [] _ = []
listXOR (x:xs) (y:ys) = binaryXOR x y:listXOR xs ys

fixLength :: [Int] -> [Int] -> ([Int], [Int])
fixLength x y
    | lx > ly = (x, fillZeros y (lx - ly))
    | lx < ly = (fillZeros x (ly - lx), y)
    | otherwise = (x, y)
    where
        lx = length x
        ly = length y

fillZeros :: [Int] -> Int -> [Int]
fillZeros xs l = replicate l 0 ++ xs

binaryXOR :: Int -> Int -> Int
binaryXOR x y
    | boolXOR (x == 1) (y == 1) = 1
    | otherwise = 0

boolXOR :: Bool -> Bool -> Bool
boolXOR True True = False
boolXOR True False = True
boolXOR False True = True
boolXOR False False = False