module Zobrist where
-- the operators of hashing (transform an occupied board positions into a single integer), could be use to represent a certain state sufficiently 

import Data.List ( elemIndex )
import Board
    ( getElement,
      goalBase,
      occupiedBoardSize,
      replace,
      startBase,
      testValidPos,
      Pos )
import Control.Monad.State ( State, evalState )
import Control.Parallel ( par, pseq )


-- a list of unique random integers 
type StateTable = [[Int]]

-- board state table: each position can only have one state, either occupied or not, and if a position is occupied, the stored value will be applied
{- 
-- the generation of the state table
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
-- the generated random number table, each entity represent an occupy state of a certain position of the internal board
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
-- the internal single-agent board for one player with the top-right the starting point and botton-left the destination
-- since this is just for heuristic board evaluation, therefore, only need to know the occupy status for each position

-- the hashed values of the both start and goal states
hashInitial :: Int
hashInitial = hash startBase
hashEnd :: Int
hashEnd = hash goalBase

-- a regular win for a player can be detected based on comparing the hashed values
winStateDetect :: [Pos] -> Bool
winStateDetect ps = hashEnd == hash ps

-- given a list of positions on the pieces on the current internal board, return a hashed value
hash :: [Pos] -> Int
hash ps = let -- filter the positions that is out of the border
              posL = filter (testValidPos occupiedBoardSize occupiedBoardSize) ps
          in  -- compute the hash value with the given random state table
              evalState (hashBoardWithPos posL) randomBoardState
    where
        hashBoardWithPos :: [Pos] -> State StateTable Int
        hashBoardWithPos [] = return 0
        hashBoardWithPos (p:ps) = do -- get the random value corresponding to the position
                                     randomState1 <- getElement p
                                     randomState2 <- hashBoardWithPos ps
                                     -- and apply XOR operator to them
                                     return (randomState1 `myXOR` randomState2)

-- given a positions change, reflect that onto the internal board's positions
flipBoard :: [Pos] -> (Pos, Pos) -> [Pos]
flipBoard ps (x, y) = case x `elemIndex` ps of -- first find the start position 
                        Nothing -> error "Not existing such position in the list"
                        Just idx -> replace idx y ps -- and replace that with the end position

--XOR Construct---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- how the XOR operator is defined

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

-- first transform the integers into binary list and then apply xor operator, and finally return as a decimal integer
myXOR :: Int -> Int -> Int
myXOR x y = toDecimal $ listXOR fx fy
    where
        (fx, fy) = let bx = toBinary x
                       by = toBinary y
                   in  bx `par` by `pseq` fixLength bx by

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

        -- xor the two binary values in lists
        listXOR :: [Int] -> [Int] -> [Int]
        listXOR _ [] = []
        listXOR [] _ = []
        listXOR (x:xs) (y:ys) = binaryXOR x y:listXOR xs ys

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
