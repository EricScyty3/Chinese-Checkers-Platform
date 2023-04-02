module Experiment where
-- setup experimental trials for testing MCTS with different strategies

import MCTS
import GameTree
import Board
import Control.Monad.State
import RBTree
import Data.Ratio
import Control.Parallel
import Data.Time
import Zobrist
import System.Environment (getArgs)
import Data.List
import System.IO
import GHC.IO
import System.Random (newStdGen)
import Extension (finalSelection)

-- during the experimental trials, several outcomes are measured under the contorl of iteration counts and time limits
-- the first is the total win rate in the multiple players game, either only two player types are invovled or the same number of the total players
-- the second is the (median) playouts, or more specific, the iterations that could be performed under the set time
-- the thrid is the (median, average) turns that takes to end the game during the experiments
-- besides, the game is also limited to avoid endless cycle, and when this is occurred, the evalutor will be applied to determine the win for each player

-- retrieve the average value of a list
mean :: [Int] -> Double
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- generate the player arrangements for allowing multiple algorithms playing againts each other
validPlayerList :: Int -> Int -> [[Int]]
validPlayerList pn pt = filter (allPlayersInvolved [0 .. pt - 1]) (playerArrangement pn pt)
    where
        -- check if all player types are involved in the generated arrangement
        allPlayersInvolved :: [Int] -> [Int] -> Bool
        allPlayersInvolved [] _ = True
        allPlayersInvolved (p:ps) arrangement = p `elem` arrangement && allPlayersInvolved ps arrangement

        -- arrangements of players in several types of certain length
        -- could exist entities missing certain types
        playerArrangement :: Int -> Int -> [[Int]]
        playerArrangement 0 _ = [[]]
        playerArrangement players playerTypes = [x:xs | x <- [0 .. (playerTypes - 1)], xs <- playerArrangement (players - 1) playerTypes]

-- a list of players to be invovled in the experimental trials
type PlayerList = [(PlayoutEvaluator, Int)] 

generatePlayerList :: Int -> PlayerList -> [PlayerList]
generatePlayerList pn ps = let pt = length ps
                               pl = validPlayerList pn pt
                           in  map (arrangeByIdx ps) pl
    where
        arrangeByIdx :: [a] -> [Int] -> [a]
        arrangeByIdx _ [] = []
        arrangeByIdx ps (i:is) = ps !! i:arrangeByIdx ps is

-- given a certain status, and run a game with different players till one of the players wins, and return the data for evaluating the performance
singleGame :: Int -> PlayerIndex -> Board -> [[Pos]] -> Int -> HistoryTrace -> (Double, Double) -> [KillerMoves] -> PlayerList -> Int -> IO (PlayerIndex, Int) 
singleGame iters pi eboard iboards pn ht cons kms pl ms = 
                                    do gen <- newStdGen
                                       (neboard, niboard, nht, nkms) <- finalSelection (GRoot 0 []) (gen, pi, 1, eboard, iboards, pn, ht, cons, sim) (Just iters, Nothing)
                                       if winStateDetect niboard then return (pi, getTurns (ms+1) pn)
                                       else let niboards = replace pi niboard iboards
                                                nextTurn = turnBase pn pi
                                            in  singleGame iters nextTurn neboard niboards pn nht cons nkms pl (ms+1)
    where
            sim = let (x, y) = pl !! pi
                in  (x, y, kms)
