module MCTS where

import GameTree
import BFS
import Board
import GHC.IO
import System.Random
import Zobrist
import Data.List
import Control.Monad.State
import Control.Monad.ST
import Data.STRef
import Control.Parallel
import System.Environment
import Data.Maybe
import RBTree (RBTree(..))

type Trace = [BoardIndex]


-- the operator of selected node record
push :: BoardIndex -> Trace -> Trace
push x xs = x:xs
pop :: Trace -> (BoardIndex, Trace)
pop xs = (last xs, init xs)

-- Normal MCTS is divided into four phases: selection, expansion, playout and backpropagation
-- the first phase is to select one resulting board with the maximum profit

-- the whole structure requires a state monad that stores the constructed game tree
-- the score is calculated based on different strategies: UCT for MCTS selection or move-based evaluation of minimax algorithm

-- create a root node based on the number of players and main board
makeRoot :: Int -> Board -> (GameTree, BoardIndex)
makeRoot players eboard = (GRoot 0 eboard (replicate players 0) [], 1)
-- create a leaf node from its parent internal node
makeLeaf :: Transform -> State GameTreeStatus GameTree
makeLeaf transform = do bi <- getBoardIdx
                        pn <- getPlayerNum
                        updateBoardIdx
                        -- add an increment of the board index, such that it won't be duplicate  
                        return (GLeaf bi transform (replicate pn 0))

-- generate a random value from 0 to 100, for certain percentage check
randomPercentage :: Int -> Bool
randomPercentage n  = unsafePerformIO (randomRIO (0, 100)) <= n
-- generate a random index given a length
randomMove :: Int -> Int
randomMove l  = unsafePerformIO (randomRIO (0, l-1))
-- handle the situation where exists more than one maximum values
randomSelection :: [Double] -> Int
randomSelection []  = error "Selection: no node for selecting"
randomSelection xs  = let is = elemIndices (maximum xs) xs
                      in  if length is == 1 then head is
                          else let ri = randomMove (length is)  -- random index of the maximum values' indices
                               in  is !! ri

-- prepend the root node to the trace 
-- select the nodes based on the strategy, and finally produce a list of traversed node
-- the move chocie could be varied, such as only choosing the non-backward moves
-- select the child nodes based on the most profits
selection :: GameTree -> Trace -> State GameTreeStatus (Trace, GameTree)
selection gametree trace = let childrenList = getChildren gametree
                           in  if null childrenList then return (trace, gametree) -- return the next node and player index for the expansion
                               else do wl <- mapM (estimateNode (getVisits gametree)) childrenList
                                       let sn = childrenList !! randomSelection wl  -- select the child with the maximum estimation 
                                           ntrace = push (getBoardIndex sn) trace
                                       nboard <- repaintBoard (getTransform sn)
                                       updatePlayerIdx
                                       updateBoard nboard
                                       selection sn ntrace  -- start the next selection at the selected node
                                              
-- expand a node based on the next possible move made by a certain player, and assign each board state an index
-- the expansion could apply various policies such as expanding multiple board states, or set a threshold of visits before any expansion
-- as well as the restriction of the expanded move's type, but the common one is to only expand once each time
expansion :: GameTree -> State GameTreeStatus GameTree
expansion n = let cs = getChildren n
              in  if not (null cs) then return n -- no need to expand if already has children, mostly expanding for leaf
                  else do co <- getPlayerColour
                          ts <- colouredMovesList co
                          cs <- mapM makeLeaf (expandPolicy co ts)
                          return (editNodeChildren cs n)
    where
        expandPolicy :: Colour -> [Transform] -> [Transform]
        expandPolicy co xs
            | not $ null front = front
            | not $ null nonback  = nonback
            | otherwise = back
            where
                front = filter ((> 0) . evaluateMove . (\(x, y) -> (projection co (getPos x), projection co (getPos y)))) xs
                nonback  = filter ((==0) . evaluateMove . (\(x, y) -> (projection co (getPos x), projection co (getPos y)))) xs
                back  = filter ((< 0) . evaluateMove . (\(x, y) -> (projection co (getPos x), projection co (getPos y)))) xs
        -- only allow frontward moves, however, this could lead to a situation where no move is avaiable

-- start with updating the root's state
-- mainBackpropagation :: PlayerIndex -> Trace -> GameTree -> GameTree -> HistoryTrace -> (GameTree, HistoryTrace)
-- mainBackpropagation pi xs r new ht = let (ts, nht) = runState (backpropagation pi xs [r] new) ht
--                                      in  (head ts, nht)

-- update the game tree stored wins for each player, as well as replacing the new expanded node
backpropagation :: PlayerIndex -> Trace -> [GameTree] -> GameTree -> State GameTreeStatus [GameTree]
backpropagation _ [] ts _ = return ts
backpropagation pi xs ts new = let (bi, ys) = pop xs
                               in  case elemIndex bi (map getBoardIndex ts) of
                                            Nothing -> error "Trace incorrect"
                                            Just li -> let ln = ts !! li
                                                       in  do pi <- getPlayerIdx
                                                              pn <- getPlayerNum
                                                              ht <- getHistoryTrace
                                                              updateHistoryTrace (editHT ln pi pn ht)
                                                              let n1 = (if getBoardIndex ln == getBoardIndex new 
                                                                        then editNodeValue pi new
                                                                        else editNodeValue pi ln)
                                                              n2 <- backpropagation pi ys (getChildren n1) new
                                                              let n3 = editNodeChildren n2 n1
                                                              return (replace li n3 ts)

-- different approaches for estimating the current board state during the playout stage
-- 1. centroid: board evaluation
-- 2. movement distance: move evaluation
-- 3. lookup table search: board evaluation

reEvaluation :: Int -> (Pos, Pos) -> Int
reEvaluation score (f, t) = score - centroidPos f + centroidPos t

-- comparing the distance to the home base
evaluateMove :: (Pos, Pos) -> Double
evaluateMove ((x1, y1), (x2, y2)) = let dist1 = sqrt (fromIntegral (x1 - x0)^2 + fromIntegral (y1 - y0)^2)
                                        dist2 = sqrt (fromIntegral (x2 - x0)^2 + fromIntegral (y2 - y0)^2)
                                    in  dist1 `par` dist2 `pseq` dist1 - dist2 -- the larger, the better
    where
        (x0, y0) = (0, 6)

-- random greedy policy with certain precentage of choosing the best option while the remaining chance of random choice
-- get the best heuristic estimated board from all expanded boards 
playoutPolicy :: Colour -> [Transform] -> State GameTreeStatus Board
playoutPolicy colour tfs = let ptfs = map (\(x, y) -> (projection colour (getPos x), projection colour (getPos y))) tfs
                               sl  = map evaluateMove ptfs
                           in if randomPercentage 95 then do let cidx = maxIndex sl
                                                                 cft  = tfs !! cidx
                                                             repaintBoard cft
                              else do let cidx = randomMove (length tfs)
                                          cft  = tfs !! cidx
                                      repaintBoard cft

-- game simulation from a certain board state 
playout :: Int -> State GameTreeStatus PlayerIndex
playout turns = do colour <- getPlayerColour
                   tfs <- colouredMovesList colour
                   nboard <- playoutPolicy colour tfs
                   pi <- getPlayerIdx
                   if winStateDetermine colour nboard then return pi
                   else do updatePlayerIdx
                           updateBoard nboard
                           playout (turns + 1)

-- should consider not only the normal wining state
-- but also the potential block state: 
-- A state in Chinese Checkers is won for player n if player n’s goal area is filled with pieces, 
-- and at least one of the pieces belongs to player n.
winStateDetermine :: Colour -> Board -> Bool
winStateDetermine c b = let ps = map (reversion c) homeBase
                            bs = evalState (do mapM getElement ps) b
                        in  isFull bs && existColour c bs 

isFull :: [BoardType] -> Bool
isFull = foldr ((&&) . isOccupied) True

existColour :: Colour -> [BoardType] -> Bool
existColour c bs = Just c `elem` map getColour bs

-- the MCTS structure that first selects the node with largest profits, then expands it, 
-- and play simulations on the expanded node, and finally update the reviewed nodes
mcts :: GameTree -> State GameTreeStatus GameTree
mcts tree = do (trace, lastnode) <- selection tree [getBoardIndex tree]
               expandednode <- expansion lastnode
               (ntrace, playnode) <- selection expandednode trace
               winIdx <- playout 0
               newTree <- backpropagation winIdx ntrace [tree] expandednode
               return (head newTree)

iterations :: GameTree -> GameTreeStatus -> Int -> (GameTree, BoardIndex, HistoryTrace)
iterations tree s@(_, bi, _, _, ht, _) 0 = (tree, bi, ht)
iterations tree s@(pi, _, board, pn, _, cons) counts = let (newTree, (_, nbi, _, _, nht, _)) = runState (mcts tree) s
                                                       in  iterations newTree (pi, nbi, board, pn, nht, cons) (counts - 1)

-- the tree could be re-used for saving computation 
-- if looking for different result, the tree could be reset every turn
finalSelection :: GameTree -> GameTreeStatus -> Int -> (Board, HistoryTrace)
finalSelection tree s@(pi, _, board, _, _, _) counts = let (ntree, _, nht) = iterations tree s counts
                                                           scores = map (averageScore pi) (getChildren ntree)
                                                       in  if null scores then error (show ntree)
                                                           else let chosenNode = getChildren ntree !! maxIndex scores
                                                            {-newRoot = GRoot (getBoardIndex chosenNode) 
                                                                            (evalState (repaintBoard (getTransform chosenNode)) s) 
                                                                            (getWins chosenNode) 
                                                                            (getChildren chosenNode)-}
                                                                in  (evalState (repaintBoard (getTransform chosenNode)) s, nht)



