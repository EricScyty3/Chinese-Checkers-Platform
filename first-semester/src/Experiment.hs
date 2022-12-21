module Experiment where
-- setup a game platform for different AI players
import MCTS
import GameTree
import Board
import Control.Monad.State
import RBTree
import Data.Ratio
import Control.Parallel
import Data.Time

-- random move player: just randomly choose a move regradless of the benefit
randomMoveDecision :: GameTreeStatus -> Board
randomMoveDecision s@(pi, _, board, pn, _, _)= let co = currentPlayerColour pi pn
                                                   bs = evalState (colouredMovesList co) s
                                                   rm = bs !! randomMove (length bs) -- randomly choose one expanded result
                                               in  evalState (repaintBoard rm) s -- return the chosen board state

-- the measurement could be different of determing the performance of the search
-- such as the win rate, how long a game is needed to end, how long a decision is needed to make, or to stop the game at certain phase and see which player is closer to win
-- or the search depth it requires to end a game

-- additional measurement could be playout related: the median number of game simulations
-- the reason why mean might not be useful here, is that the wider sampling causes fewer simulations during the playout


-- given a set of agruments, test the performance of the selection strategy
-- measurement: median playouts
experiment1 _ _ [] _ = []
experiment1 pn board (a:as) iter = let p = mean (experimentalPlayout pn board a iter) -- get the average of a list of median playouts
                                       ps = experiment1 pn board as iter
                                   in  p `par` ps `pseq` p:ps

-- given a board state with the same configuration and check the median amount of turns it needs to complete a playout
experimentalPlayout _ _ _ 0 = []
experimentalPlayout pn board cons counts = let (root, rootIdx) = makeRoot pn board
                                               (_, _, pl) = finalSelection root (0, rootIdx, board, pn, RBLeaf, cons) 1000
                                               rs = experimentalPlayout pn board cons (counts - 1)
                                           in  pl `par` rs `pseq` medianValue pl:rs
{-
    -- given a multiple-players board, let certain players play against each other and see how well the players perform
    -- measurments: avarage win rate, average game turns
    experiment2 _ _ [] _ = ([], [])
    experiment2 pn board (a:as) iter = let (wins, turns) = experimentalGameS pn board a iter
                                        meanWins = mean wins
                                        meanTurns = mean (map fromIntegral turns)
                                        (mws, mts) = experiment2 pn board as iter
                                    in  (meanWins, meanTurns) `par` (mws, mts) `pseq` (meanWins:mws, meanTurns:mts)

    -- play a match several times with same configuration
    experimentalGameS _ _ _ 0 = ([], [])
    experimentalGameS pn board cons counts = let (win, turns) = experimentalGame 0 pn board cons [2] 1 10
                                                (ws, ts) = experimentalGameS pn board cons (counts - 1)
                                            in  (win, turns) `par` (ws, ts) `pseq` (win:ws, turns:ts)


    -- a match between two types of players: random player and MCTS player, to test the performance based on win rate, game turns, and playouts
    experimentalGame pi pn board cons mctsPi moves iter =
                                                        let colour = currentPlayerColour pi pn
                                                        in  if pi `notElem` mctsPi then let newBoard = randomMoveDecision (pi, 0, board, pn, RBLeaf, cons)
                                                                                        in  if winStateDetermine colour newBoard then {-do printEoard newBoard 
                                                                                                                                        return-} (0, getTurns moves pn)
                                                                                            else -- do -- printEoard newBoard
                                                                                                    experimentalGame (turnBase pn pi) pn newBoard cons mctsPi (moves+1) iter
                                                            else let (root, rootIdx) = makeRoot pn board
                                                                    (newBoard, _, _) = finalSelection root (pi, rootIdx, board, pn, RBLeaf, cons) iter
                                                                in  if winStateDetermine colour newBoard then {-do printEoard newBoard 
                                                                                                                return-} (1, getTurns moves pn)
                                                                    else -- do -- printEoard newBoard
                                                                            experimentalGame (turnBase pn pi) pn newBoard cons mctsPi (moves+1) iter
-}
-- ghc -main-is Experiment Experiment.hs -O2 -outputdir dist
main = do start <- getCurrentTime
          let xs = experiment1 3 (eraseBoard threePlayersSet) [(5, 0.5)] 1
          print xs
          end <- getCurrentTime
          print $ diffUTCTime end start

-- systematic test for optimising the parameters for game tree evaluation
settings :: [(Double, Double)]
settings = [(x, y) | x <- map fromRational [0 .. 5], y <- map fromRational [0 .. 5]]
