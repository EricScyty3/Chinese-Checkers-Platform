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
mean xs = realToFrac (sum xs) / genericLength xs

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
experiment1 :: Int -> Board -> [(Double, Double)] -> Int -> [Double]
experiment1 _ _ [] _ = []
experiment1 pn board (a:as) iter = let t = mean (take iter (multipleCalls pn board a))
                                       rs = experiment1 pn board as iter
                                   in  t `par` rs `pseq` t:rs
-- repeat calling the MCTS function several times such that is could be statistically meaningful 
-- the less it is, the faster it converges during the playouts
multipleCalls :: Int -> [[BoardPos]] -> (Double, Double) -> [Int]
multipleCalls pn board cons = let ct = convergeTurn (singleCall pn board cons)
                              in  ct:multipleCalls pn board cons
-- given a board state, call the MCTS function for deciding the next move, and return the turns of game simulatons taken in the playout phase
singleCall :: Int -> Board -> (Double, Double) -> [Int]
singleCall pn board cons = let (root, rootIdx) = makeRoot pn board
                               (_, _, _, pl) = finalSelection root (0, rootIdx, board, pn, RBLeaf, cons) 0 500
                           in  pl

-- play games with the random players with different parameters of selection strategy
experiment2 :: Int -> Board -> [(Double, Double)] ->  [Int] -> Int -> ([Double], [Double])
experiment2 _ _ [] _ _ = ([], [])
experiment2 pn board (a:as) mctsPi iter = let r@(ws, ts) = multipleGames (0, 0, board, pn, RBLeaf, a) mctsPi 1 iter
                                              winRate = mean ws
                                              averageTurns = mean ts
                                              rs@(ws', ts') = experiment2 pn board as mctsPi iter
                                          in  r `par` rs `pseq` (winRate:ws', averageTurns:ts')
-- repeat the game with the random player several times
multipleGames :: GameTreeStatus -> [Int] -> Int -> Int -> ([Int], [Int])
multipleGames _ _ _ 0 = ([], [])
multipleGames s@(pi, bi, board, pn, ht, cons) mctsPi gs iter = let r@(win, gt) = singleGame s mctsPi gs
                                                                   rs@(wins, gts) = multipleGames s mctsPi gs (iter - 1)
                                                               in  r `par` rs `pseq` (win:wins, gt:gts)
-- given a pair of parameters for MCTS, and hold a game againt the random-choice player
-- return the winning player, game turns
singleGame :: GameTreeStatus -> [Int] -> Int -> (Int, Int)
singleGame s@(pi, bi, board, pn, ht, cons) mctsPi gs = let colour = playerColour pi pn
                                                       in  if pi `notElem` mctsPi then let newBoard = randomMoveDecision s
                                                                                       in  if winStateDetermine colour newBoard then (0, getTurns gs pn) -- 0 if MCTS loses
                                                                                           else singleGame (turnBase pn pi, bi, newBoard, pn, ht, cons) mctsPi (gs+1)
                                                           else let (root, rootIdx) = makeRoot pn board
                                                                    (newBoard, _, nht, _) = finalSelection root (pi, rootIdx, board, pn, ht, cons) 0 10
                                                                in  if winStateDetermine colour newBoard then (1, getTurns gs pn)
                                                                    else singleGame (turnBase pn pi, bi, newBoard, pn, nht, cons) mctsPi (gs+1)

-- ghc -main-is Experiment Experiment.hs -O2 -fllvm -outputdir dist
main = do arg <- getArgs
          start <- getCurrentTime
          let iter = read (head arg)
              -- xs = experiment1 3 (eraseBoard threePlayersSet) hashInitial [(5, 0.5)] iter
              (ws, ts) = experiment2 3 (eraseBoard threePlayersSet) [(5, 0.5)] [2] iter
          print ws
          print ts
          end <- getCurrentTime
          print $ diffUTCTime end start

-- systematic test for optimising the parameters for game tree evaluation
settings :: [(Double, Double)]
settings = [(x, y) | x <- map fromRational [0 .. 5], y <- map fromRational [0 .. 5]]
