module Experiment where
import MCTS
import GameTree
import Board
import Control.Monad.State
import RBTree
import Data.Ratio
import Control.Parallel

-- evaluating the parameters of MCTS selection
-- first set up the gaming platform that evatually output the winners' index

-- random move player
-- randomly choose a move of the current player
randomMoveDecision :: GameTreeStatus -> Board
randomMoveDecision s@(pi, _, board, pn, _, _)= let co = currentPlayerColour pi pn
                                                   bs = evalState (colouredMovesList co) s
                                                   rm = bs !! randomMove (length bs) -- randomly choose one expanded result
                                               in  evalState (repaintBoard rm) s

-- assuming random player plays first
experimentalGame s@(pi, bi, board, pn, ht, cons) c = let colour = currentPlayerColour pi pn
                                                     in  if pi /= 2 then let newBoard = randomMoveDecision s
                                                                         in  if winStateDetermine colour newBoard then (0, c+1) {- do printEoard newBoard
                                                                                                                          putStrLn ("Winner: " ++ show pi)-}
                                                                             else -- do printEoard newBoard
                                                                                    experimentalGame (turnBase pn pi, bi, newBoard, pn, ht, cons) (c+1)
                                                         else let (root, rootIdx) = makeRoot pn board
                                                                  (newBoard, nht) = finalSelection root (pi, rootIdx, getRootBoard root, pn, ht, cons) 10
                                                              in  if winStateDetermine colour newBoard then (1, c+1) {- do printEoard newBoard
                                                                                                               putStrLn ("Winner: " ++ show pi)-}
                                                                  else -- do printEoard newBoard
                                                                          experimentalGame (turnBase pn pi, bi, newBoard, pn, nht, cons) (c+1)


experimentSets :: (Double, Double) -> Int -> ([Int], [Int])
experimentSets _ 0 = ([], [])
experimentSets cons counts = let r@(winIdx, turns) = experimentalGame (0, 0, eraseBoard threePlayersSet, 3, RBLeaf, cons) 0
                                 rs@(ws, ts) = experimentSets cons (counts - 1)
                             in  r `par` rs `pseq` (winIdx:ws, turns:ts)

mean xs = fromIntegral (sum xs) / fromIntegral (length xs)


main = print $ testSet settings 50

-- systematic test for optimising the parameters for game tree evaluation
settings :: [(Double, Double)]
settings = [(x, y) | x <- map fromRational [0 .. 5], y <- map fromRational [0 .. 5]]

-- ((4.0,5.0),139.8),((5.0,1.0),139.5)

-- main = let testSet = map (`experimentSets` 10) settings
--            ws = winRate (zip testSet settings)
--            cs = meanTurns (zip testSet settings)
--        in  do ws `par` cs `pseq` print ws 
--               print cs
--               -- print (zip settings (meanTurns testSet))
--     where

testSet [] _ = ([], [])
testSet (p:ps) setSize = let r@(winRate, averageTurns) = (\(x, y) -> (mean x, mean y)) (experimentSets p setSize)
                             rs@(ws, ts) = testSet ps setSize
                         in  r `par` rs `pseq` (winRate:ws, averageTurns:ts)
                                 