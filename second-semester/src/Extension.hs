module Extension where
-- a module that allows controls of conditions such as iterations and time limits to be applied to the MCTS

import MCTS ( mcts, randomMaxSelection )
import GameTree
    ( averageScore,
      getChildren,
      getPlayerIdx,
      getTransform,
      playerColour,
      repaintBoard,
      BoardIndex,
      GameTree (GRoot),
      GameTreeStatus,
      HistoryTrace,
      KillerMoves, setRandGen, getWins, getVisits, PlayerIndex, Wins)
import Board ( projectMove, Board, Pos, repaintPath, eraseBoard, playerColourList, externalBoard, startBase, printEoard )
import Data.Time
    ( UTCTime, nominalDiffTimeToSeconds, diffUTCTime, getCurrentTime )
import Control.Monad.State ( evalState, runState )
import Zobrist ( flipBoard )
import Control.Parallel ( par, pseq )
import Data.Fixed ( Pico )
import System.Random ( newStdGen )
import Data.List (elemIndices)
import RBTree (RBTree(RBLeaf))
import System.Environment ( getArgs )
import Configuration (lookupTable)

-- the condition for stopping the search of MCTS
type MCTSControl = (Maybe Int, Maybe Double)

-- given a search tree and game state, with the control of tree search, return the optimal result that is received by MCTS 
-- currently, only the iteration counts and time limits are considered, the expansion threshold could be extended but not necessary in here
finalSelection :: GameTree -> GameTreeStatus -> MCTSControl -> IO (Board, [Pos], HistoryTrace)
finalSelection tree s@(_, pi, _, eboard, iboards, pn, _, _, _) control =
                                                                   do -- pass the arguments to the decision function controlled by certain threshold 
                                                                      -- get the new search tree and the new movement history
                                                                      (ntree, nht) <- getResultsUnderControl tree s control
                                                                      let children = getChildren ntree
                                                                      if null children then do printEoard eboard
                                                                                               error "No effective result was retrieved"

                                                                      else do gen <- newStdGen -- get a new random nunmber generator for selecting the returned move
                                                                              let -- get the win rate of entered player for investigated movements
                                                                                  scores = map (\n -> averageScore pi (getWins n) (getVisits n)) children
                                                                                  (_, newRandomState) = runState (setRandGen gen) s
                                                                                  -- get the maximum win rate move (child) as the next movement
                                                                                  -- if there exist multiple maximum scores, then randomly choose one of them
                                                                                  randMaxIdx = evalState (randomMaxSelection scores) newRandomState
                                                                                  chosenNode = children !! randMaxIdx
                                                                                  -- return the resulting decision
                                                                                  colour = playerColour pi pn
                                                                                  move = getTransform chosenNode
                                                                                  newBoard = repaintPath eboard move
                                                                                  pmove = move `par` colour `pseq` projectMove colour move
                                                                                  newInternalState = flipBoard (iboards !! pi) pmove
                                                                              -- return the new external and internal boards, as well as the movement history  
                                                                              return $ newBoard `par` newInternalState `pseq` (newBoard, newInternalState, nht)
                                                                              
-- compute the MCTS with certain threshold to control the progress
getResultsUnderControl :: GameTree -> GameTreeStatus -> MCTSControl -> IO (GameTree, HistoryTrace)
getResultsUnderControl tree status (Just iters, Nothing) = do iterations tree status iters -- run MCTS with certain iterations
getResultsUnderControl tree status (Nothing, Just seconds) = do -- first get the current time for later check
                                                                startTime <- getCurrentTime
                                                                -- run the MCTS with certain time limits
                                                                timeLimits tree status (startTime, realToFrac seconds)
getResultsUnderControl tree status _ = error "Invalid control"

-- repeat the four MCTS phases until certain iterations are reached
iterations :: GameTree -> GameTreeStatus -> Int -> IO (GameTree, HistoryTrace)
-- return when countdown to zero
iterations tree s@(_, _, _, _, _, _, ht, _, _) 0 = return (tree, ht)
iterations tree s@(_, pi, bi, board, ps, pn, ht, cons, evaluator) count =
    -- reset every status while maintaining the board index and move history
    let n@(newGen, newTree, newIdx, newHistory) = evalState (mcts tree) s
    -- inherit the movement history, and record the playout turns, decrement the count
    in  n `seq` iterations newTree (newGen, pi, newIdx, board, ps, pn, newHistory, cons, evaluator) (count-1)

-- repeating the MCTS until certain time setting (in seconds) are reached
timeLimits :: GameTree -> GameTreeStatus -> (UTCTime, Pico) -> IO (GameTree, HistoryTrace)
timeLimits tree s@(_, pi, bi, board, ps, pn, ht, cons, evaluator) (start, duration) =
    let n@(newGen, newTree, newIdx, newHistory) = evalState (mcts tree) s
    in  do -- check if the time is exceeded
           currentTime <- n `seq` getCurrentTime
           let interval = nominalDiffTimeToSeconds $ diffUTCTime currentTime start
           if interval >= duration then return (newTree, newHistory)
           -- otherwise, keep processing
           else timeLimits newTree (newGen, pi, newIdx, board, ps, pn, newHistory, cons, evaluator) (start, duration)
