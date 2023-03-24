module Extension where
-- an additional module that allows controls of other conditions such as expanded nodes and the time limit
import MCTS
import GameTree
import Board
import GHC.IO
import Data.Time
import Control.Monad.State
import Zobrist
import Control.Parallel
import RBTree
import System.Environment
import Configuration
import Data.Fixed

-- ghc -main-is Extension Extension.hs -O2 -fllvm -outputdir dist
main = do arg <- getArgs
          start <- lookupTable `seq` getCurrentTime
          let eval = read $ head arg
              depth = read $ arg !! 1
              control = read $ arg !! 2
              turns = testRun eval depth control
          print turns
          -- printEoard board
          end <- getCurrentTime
          print $ "Time cost: " ++ show (diffUTCTime end start)

testRun eval depth control = let (_, _, _, turns) = finalSelection (GRoot 0 []) 
                                                    (0, 1, eraseBoard (playerColourList pn) externalBoard, replicate pn startBase, pn, RBLeaf, (3, 1), 
                                                    (eval, depth, replicate pn [])) 
                                                    control
                             in turns
    where 
        pn = 3

finalSelection :: GameTree -> GameTreeStatus -> (Maybe Int, Maybe Int, Maybe Pico) -> (Board, [Pos], HistoryTrace, [Int])
finalSelection tree s@(pi, _, _, iboard, pn, _, _, _) control =
                                                               let (ntree, scores, nht, playoutTurns) = getResultsUnderControl tree s control
                                                               in  if null scores then error (show ntree)
                                                                   else let -- get the maximum win rate move as the next movement
                                                                            chosenNode = getChildren ntree !! randomMaxSelection scores
                                                                            -- return the resulting decision
                                                                            colour = playerColour pi pn
                                                                            (from, to) = getTransform chosenNode
                                                                            newBoard = evalState (repaintBoard (from, to)) s
                                                                            (pfrom, pto) = (from, to) `par` colour `pseq` projectMove colour (from ,to)
                                                                            newInternalState = flipBoard (iboard !! pi) (pfrom, pto)
                                                                        in  newBoard `par` newInternalState `pseq` (newBoard, newInternalState, nht, playoutTurns)

getResultsUnderControl :: GameTree -> GameTreeStatus -> (Maybe Int, Maybe Int, Maybe Pico) -> (GameTree, [Double], HistoryTrace, [Int])
getResultsUnderControl tree status (Just iters, Nothing, Nothing) = let (ntree, _, nht, playoutTurns) = iterations tree status [] iters
                                                                        pi = evalState getPlayerIdx status
                                                                        scores = ntree `par` pi `pseq` map (averageScore pi) (getChildren ntree) -- list all win rate for a player
                                                                    in  (ntree, scores, nht, playoutTurns)

getResultsUnderControl tree status (Nothing, Nothing, Just seconds) = let startTime = unsafePerformIO getCurrentTime
                                                                          (ntree, _, nht, playoutTurns) = unsafePerformIO $ timeLimits tree status [] (startTime, seconds)
                                                                          pi = evalState getPlayerIdx status
                                                                          scores = ntree `par` pi `pseq` map (averageScore pi) (getChildren ntree) -- list all win rate for a player
                                                                      in  (ntree, scores, nht, playoutTurns)   

getResultsUnderControl tree status (Nothing, Just nodes, Nothing) = let (ntree, _, nht, playoutTurns) = expansionLimits tree status [] nodes
                                                                        pi = evalState getPlayerIdx status
                                                                        scores = ntree `par` pi `pseq` map (averageScore pi) (getChildren ntree) -- list all win rate for a player
                                                                    in  (ntree, scores, nht, playoutTurns)

getResultsUnderControl tree status _ = error "More than two controls are added"


-- repeating the MCTS until certain time setting (in seconds) are reached
timeLimits :: GameTree -> GameTreeStatus -> [Int] -> (UTCTime, Pico) -> IO (GameTree, BoardIndex, HistoryTrace, [Int])
timeLimits tree s@(pi, bi, board, ps, pn, ht, cons, (eval, depth, _)) playoutTurns (start, duration) = 
    do currentTime <- getCurrentTime
       let interval = nominalDiffTimeToSeconds $ diffUTCTime currentTime start
       if interval >= duration then return (tree, bi, ht, reverse playoutTurns)
       else let r@(newTree, newIdx, newHistory, turns, kms) = evalState (mcts tree) s -- force the evaluation to be done here 
            in  r `seq` timeLimits newTree (pi, newIdx, board, ps, pn, newHistory, cons, (eval, depth, kms)) (turns:playoutTurns) (start, duration)

expansionLimits :: GameTree -> GameTreeStatus -> [Int] -> Int -> (GameTree, BoardIndex, HistoryTrace, [Int])
expansionLimits tree s@(pi, bi, board, ps, pn, ht, cons, (eval, depth, _)) playoutTurns nodes = 
    if bi >= nodes then (tree, bi, ht, reverse playoutTurns)
    else let (newTree, newIdx, newHistory, turns, kms) = evalState (mcts tree) s
         in  expansionLimits newTree (pi, newIdx, board, ps, pn, newHistory, cons, (eval, depth, kms)) (turns:playoutTurns) nodes
