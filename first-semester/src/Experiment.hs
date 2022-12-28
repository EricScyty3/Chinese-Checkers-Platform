module Experiment where
-- setup a draft experiment set for different AI players
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

-- retrieve the average value of a list
mean :: [Int] -> Double
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- retrieve the median value of a list (regardless of the amount of certain value)
medianValue :: [Int] -> Double
medianValue [] = 0
medianValue [x] = fromIntegral x
medianValue [x, y] = fromIntegral (x+y) / 2
medianValue xs = let minIdx = elemIndices (minimum xs) xs
                     maxIdx = elemIndices (maximum xs) xs
                     takeList = take (head maxIdx) xs
                     dropList = drop (last minIdx + 1) takeList
                 in  if null dropList then medianValue [minimum xs, maximum xs]
                     else medianValue dropList

-- get the turn where the algorithm that converges to 0 turn
convergeTurn :: [Int] -> Int
convergeTurn [] = 0 -- when all turns are 0
convergeTurn xs = let x = last xs
                  in  if x /= 0 then length xs -- when the input length is excatly equal to the output, meaning that the convergence is not reached
                      else convergeTurn (init xs)

-- random move player: just randomly choose a move regradless of the benefit
randomMoveDecision :: GameTreeStatus -> Board
randomMoveDecision s@(pi, _, board, pn, _, _)= let co = playerColour pi pn
                                                   bs = evalState (colouredMovesList co) s
                                                   ns = expandPolicy co bs -- the backward movement should be restricted, otherwise, no progress might be made
                                                   rm = bs !! randomMove (length bs) -- randomly choose one expanded result
                                               in  evalState (repaintBoard rm) s -- return the chosen board state

-- given a set of agruments, test the performance of the selection strategy
experiment1 :: Int -> Board -> Int -> [(Double, Double)] -> Int -> [Double]
experiment1 _ _ _ [] _ = []
experiment1 pn board bh (a:as) iter = let t = mean (take iter (multipleCalls pn board bh a))
                                          rs = experiment1 pn board bh as iter
                                      in  t `par` rs `pseq` t:rs

-- repeat calling the MCTS function several times such that is could be statistically meaningful 
-- the less it is, the faster it converges
multipleCalls :: Int -> [[BoardPos]] -> Int -> (Double, Double) -> [Int]
multipleCalls pn board bh cons = let ct = convergeTurn (singleCall pn board bh cons)
                                 in  ct:multipleCalls pn board bh cons

-- given a board state, call the MCTS function for deciding the next move, and return the turns of game simulatons taken in the playout phase
singleCall :: Int -> Board -> Int -> (Double, Double) -> [Int]
singleCall pn board bh cons = let (root, rootIdx) = makeRoot pn board
                                  (_, _, _, pl) = finalSelection root (0, rootIdx, board, pn, RBLeaf, cons) bh 500
                              in  pl

-- ghc -main-is Experiment Experiment.hs -O2 -fllvm -outputdir dist
main = do arg <- getArgs
          start <- getCurrentTime
          let iter = read (head arg)
              xs = experiment1 3 (eraseBoard threePlayersSet) hashInitial [(5, 0.5)] iter
          print xs
          end <- getCurrentTime
          print $ diffUTCTime end start

-- systematic test for optimising the parameters for game tree evaluation
settings :: [(Double, Double)]
settings = [(x, y) | x <- map fromRational [0 .. 5], y <- map fromRational [0 .. 5]]
