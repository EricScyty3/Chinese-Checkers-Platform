module Extension where
-- a module that allows controls of conditions such as iterations and time limits to be applied to the MCTS

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

-- given a search tree and game state, with the control of tree search, return the optimal result that is received by MCTS 
-- currently, only the iteration counts and time limits are considered, the expansion threshold could be extended but not necessary in here
finalSelection :: GameTree -> GameTreeStatus -> (Maybe Int, Maybe Pico) -> IO (Board, [Pos], HistoryTrace, [KillerMoves], Int)
finalSelection tree s@(_, pi, _, eboard, iboards, pn, _, _, _) control =
                                                                   do -- pass the arguments to the decision function controlled by certain threshold 
                                                                      -- return the new search tree, the scores for the possible expansion of current game state
                                                                      -- the new movement history, and the counts of game simulations' turns
                                                                      (ntree, scores, nht, kms, trials) <- getResultsUnderControl tree s control
                                                                      if null scores then do printEoard eboard
                                                                                             error "No effective result was retrieved"
                                                                                             
                                                                      else do gen <- newStdGen
                                                                              let (_, newState) = runState (setRandGen gen) s
                                                                                  -- get the maximum win rate move (child) as the next movement
                                                                                  -- if there exist multiple maximum scores, then randomly choose one of them
                                                                                  randMaxIdx = evalState (randomMaxSelection scores) newState
                                                                                  chosenNode = getChildren ntree !! randMaxIdx
                                                                                  -- return the resulting decision
                                                                                  colour = playerColour pi pn
                                                                                  move = getTransform chosenNode
                                                                                  newBoard = repaintPath eboard move
                                                                                  pmove = move `par` colour `pseq` projectMove colour move
                                                                                  newInternalState = flipBoard (iboards !! pi) pmove
                                                                              return $ newBoard `par` newInternalState `pseq` (newBoard, newInternalState, nht, kms, trials)
-- compute the MCTS with certain threshold to control the progress
getResultsUnderControl :: GameTree -> GameTreeStatus -> (Maybe Int, Maybe Pico) -> IO (GameTree, [Double], HistoryTrace, [KillerMoves], Int)
getResultsUnderControl tree status@(_, pi, _, _, _, _, _, _, _) (Just iters, Nothing) =
                                                           do -- retrieve the result from running MCTS with certain iterations
                                                              (ntree, _, nht, kms) <- iterations tree status iters
                                                              -- list all win rates for the current player of all expanded moves
                                                              let scores = getWinRates pi (getChildren ntree)
                                                              return (ntree, scores, nht, kms, iters)

getResultsUnderControl tree status@(_, pi, _, _, _, _, _, _, _) (Nothing, Just seconds) =
                                                             do -- first get the current time for later check
                                                                startTime <- getCurrentTime
                                                                -- run the MCTS with certain time limits
                                                                (ntree, _, nht, kms, iters) <- timeLimits tree status (startTime, seconds) 0
                                                                let scores = getWinRates pi (getChildren ntree) -- list all win rate for a player
                                                                return (ntree, scores, nht, kms, iters)
getResultsUnderControl tree status _ = error "Invalid control"

-- get the win rates for all children of a node
getWinRates :: PlayerIndex -> [GameTree] -> [Double]
getWinRates _ [] = []
getWinRates pi (n:ns) = let wins = getWins n
                            visits = sum wins
                        in  averageScore pi wins visits : getWinRates pi ns

-- repeat the MCTS until certain iterations are reached
iterations :: GameTree -> GameTreeStatus -> Int -> IO (GameTree, BoardIndex, HistoryTrace, [KillerMoves])
-- return when countdown to zero
iterations tree s@(_, _, bi, _, _, _, ht, _, (_, _, kms)) 0 = return (tree, bi, ht, kms)
iterations tree s@(_, pi, bi, board, ps, pn, ht, cons, (eval, depth, _)) count =
    -- reset every status while maintaining the board index and move history
    let n@(newGen, newTree, newIdx, newHistory, kms) = evalState (mcts tree) s
    -- inherit the movement history, and record the playout turns, decrement the count
    in  n `seq` iterations newTree (newGen, pi, newIdx, board, ps, pn, newHistory, cons, (eval, depth, kms)) (count-1)

-- repeating the MCTS until certain time setting (in seconds) are reached
timeLimits :: GameTree -> GameTreeStatus -> (UTCTime, Pico) -> Int -> IO (GameTree, BoardIndex, HistoryTrace, [KillerMoves], Int)
timeLimits tree s@(_, pi, bi, board, ps, pn, ht, cons, (eval, depth, kms)) (start, duration) counts =
    do -- check if the time is exceeded
       currentTime <- getCurrentTime
       let interval = nominalDiffTimeToSeconds $ diffUTCTime currentTime start
       if interval >= duration then return (tree, bi, ht, kms, counts)
       -- otherwise, keep processing
       else let n@(newGen, newTree, newIdx, newHistory, nkms) = evalState (mcts tree) s
            in  n `seq` timeLimits newTree (newGen, pi, newIdx, board, ps, pn, newHistory, cons, (eval, depth, nkms)) (start, duration) (counts+1)
