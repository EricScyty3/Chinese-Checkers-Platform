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
import System.Random

-- ghc -main-is Extension Extension.hs -O2 -fllvm -outputdir dist
{-
main = do arg <- getArgs
          start <- lookupTable `seq` getCurrentTime
          let eval = read $ head arg
              depth = read $ arg !! 1
              control = read $ arg !! 2
          turns <- testRun eval depth control
          print turns
          -- printEoard board
          end <- getCurrentTime
          print $ "Time cost: " ++ show (diffUTCTime end start)

testRun eval depth control = do gen <- newStdGen
                                (_, _, _, turns) <- finalSelection (GRoot 0 [])
                                                    (gen, 0, 1, eraseBoard (playerColourList pn) externalBoard, replicate pn startBase, pn, RBLeaf, (3, 1),
                                                    (eval, depth, replicate pn []))
                                                    control
                                return turns
    where
        pn = 3
-}
finalSelection :: GameTree -> GameTreeStatus -> (Maybe Int, Maybe Int, Maybe Pico) -> IO (Board, [Pos], HistoryTrace, [Int], [KillerMoves])
finalSelection tree s@(_, pi, _, _, iboard, pn, _, _, _) control =
                                                               do (ntree, scores, nht, playoutTurns, kms) <- getResultsUnderControl tree s control
                                                                  if null scores then error (show ntree)
                                                                  else do randIdx <- randomRIO (0, length scores - 1)
                                                                          let -- get the maximum win rate move as the next movement
                                                                              chosenNode = getChildren ntree !! randIdx
                                                                               -- return the resulting decision
                                                                              colour = playerColour pi pn
                                                                              (from, to) = getTransform chosenNode
                                                                              newBoard = evalState (repaintBoard (from, to)) s
                                                                              (pfrom, pto) = (from, to) `par` colour `pseq` projectMove colour (from ,to)
                                                                              newInternalState = flipBoard (iboard !! pi) (pfrom, pto)
                                                                          return $ newBoard `par` newInternalState `pseq` (newBoard, newInternalState, nht, playoutTurns, kms)

getResultsUnderControl :: GameTree -> GameTreeStatus -> (Maybe Int, Maybe Int, Maybe Pico) -> IO (GameTree, [Double], HistoryTrace, [Int], [KillerMoves])
getResultsUnderControl tree status (Just iters, Nothing, Nothing) = do (ntree, _, nht, playoutTurns, kms) <- iterations tree status [] iters
                                                                       let pi = evalState getPlayerIdx status
                                                                           scores = ntree `par` pi `pseq` map (averageScore pi) (getChildren ntree) -- list all win rate for a player
                                                                       return (ntree, scores, nht, playoutTurns, kms)

getResultsUnderControl tree status (Nothing, Nothing, Just seconds) = do startTime <- getCurrentTime
                                                                         (ntree, _, nht, playoutTurns, kms) <- timeLimits tree status [] (startTime, seconds)
                                                                         let pi = evalState getPlayerIdx status
                                                                             scores = ntree `par` pi `pseq` map (averageScore pi) (getChildren ntree) -- list all win rate for a player
                                                                         return (ntree, scores, nht, playoutTurns, kms)

getResultsUnderControl tree status (Nothing, Just nodes, Nothing) = do (ntree, _, nht, playoutTurns, kms) <- expansionLimits tree status [] nodes
                                                                       let pi = evalState getPlayerIdx status
                                                                           scores = ntree `par` pi `pseq` map (averageScore pi) (getChildren ntree) -- list all win rate for a player
                                                                       return (ntree, scores, nht, playoutTurns, kms)

getResultsUnderControl tree status _ = error "More than two controls are added"


-- repeating the MCTS until certain time setting (in seconds) are reached
timeLimits :: GameTree -> GameTreeStatus -> [Int] -> (UTCTime, Pico) -> IO (GameTree, BoardIndex, HistoryTrace, [Int], [KillerMoves])
timeLimits tree s@(_, pi, bi, board, ps, pn, ht, cons, (eval, depth, kms)) playoutTurns (start, duration) =
    do currentTime <- getCurrentTime
       let interval = nominalDiffTimeToSeconds $ diffUTCTime currentTime start
       if interval >= duration then return (tree, bi, ht, reverse playoutTurns, kms)
       else let r@(newGen, newTree, newIdx, newHistory, turns, kms) = evalState (mcts tree) s -- force the evaluation to be done here 
            in  r `seq` timeLimits newTree (newGen, pi, newIdx, board, ps, pn, newHistory, cons, (eval, depth, kms)) (turns:playoutTurns) (start, duration)

expansionLimits :: GameTree -> GameTreeStatus -> [Int] -> Int -> IO (GameTree, BoardIndex, HistoryTrace, [Int], [KillerMoves])
expansionLimits tree s@(_, pi, bi, board, ps, pn, ht, cons, (eval, depth, kms)) playoutTurns nodes =
    if bi >= nodes then return (tree, bi, ht, reverse playoutTurns, kms)
    else let (newGen, newTree, newIdx, newHistory, turns, kms) = evalState (mcts tree) s
         in  expansionLimits newTree (newGen, pi, newIdx, board, ps, pn, newHistory, cons, (eval, depth, kms)) (turns:playoutTurns) nodes
