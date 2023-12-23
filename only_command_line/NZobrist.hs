module NZobrist where
-- the operators of hashing (transform an occupied board positions into a single integer), could be use to represent a certain state sufficiently 

import Data.List ( elemIndex )
import NBoard ( Pos, occupiedBoardSize )
import Control.Monad.State ( State, evalState )
import Control.Parallel ( par, pseq )
import qualified Data.HashMap.Strict as HM
import Data.Bits ( Bits(xor) )
import NProjection (startBase, goalBase)


-- a list of unique random integers 
type StateTable = HM.HashMap Pos Int

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
randomBoardState :: [(Pos, Int)]
randomBoardState = [
                    ((0,0),3292324401),((0,1),233489048),((0,2),2624610400),((0,3),1597242128),((0,4),1980104913),((0,5),1321694675),((0,6),3441975186),
                    ((1,0),1130700312),((1,1),1305326220),((1,2),2205018208),((1,3),2326285813),((1,4),2296381747),((1,5),3769793212),((1,6),2531570566),
                    ((2,0),3207055759),((2,1),1137426218),((2,2),2956685049),((2,3),4256428639),((2,4),724082013),((2,5),2138168924),((2,6),2728019182),
                    ((3,0),2087020672),((3,1),2189657155),((3,2),903285258),((3,3),1992067404),((3,4),2726019740),((3,5),1298665595),((3,6),1408913945),
                    ((4,0),2990988946),((4,1),3063264481),((4,2),149517643),((4,3),1100318883),((4,4),2752873187),((4,5),3781215980),((4,6),2792287776),
                    ((5,0),977698729),((5,1),118633436),((5,2),2784537123),((5,3),1886397907),((5,4),1695135422),((5,5),92683337),((5,6),2971222636),
                    ((6,0),1857154033),((6,1),3253362046),((6,2),1756536471),((6,3),2064999353),((6,4),510226296),((6,5),402957728),((6,6),3185258486)
                ]

-- rearrange the board as a hashmap 
stateTable :: StateTable
stateTable = HM.fromList randomBoardState

--Hash Operators---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- the internal single-agent board for one player with the top-right the starting point and botton-left the destination
-- since this is just for heuristic board evaluation, therefore, only need to know the occupy status for each position

-- the hashed values of the both start and goal states
hashInitial :: Int
hashInitial = hashBoard startBase
hashEnd :: Int
hashEnd = hashBoard goalBase

-- a regular win for a player can be detected based on comparing the hashed values
winStateDetect :: [Pos] -> Bool
winStateDetect ps = hashEnd == hashBoard ps

-- change the current hashed value based on the new movement made on the board
updateHash :: Int -> (Pos, Pos) -> Int
updateHash h (start, end) = case HM.lookup start stateTable of 
                                Nothing -> error "Hash: invalid start position"
                                Just h1 -> case HM.lookup end stateTable of
                                               Nothing -> error "Hash: invalid end position"
                                               Just h2 -> (h `xor` h1) `xor` h2

-- given a list of positions on the pieces on the current internal board, return a hashed value
hashBoard :: [Pos] -> Int
hashBoard [] = 0
hashBoard (p:ps) = case HM.lookup p stateTable of
                        -- get the random value corresponding to the position
                        -- and apply XOR operator to them
                        Just h  -> h `xor` hashBoard ps
                        Nothing -> error "Hash: invalid board position"

-- given a positions change, reflect that onto the internal board's positions
flipBoard :: [Pos] -> (Pos, Pos) -> [Pos]
flipBoard ps (start, end) = replace end start ps
    where
        replace _ _ [] = []
        replace new old (x:xs) = if old == x then new:xs
                                 else x:replace new old xs