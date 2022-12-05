module MCTS where

import GameTree
import State
import ShortestPath
import Board
import GHC.IO
import System.Random
import Zobrist
import Data.List

type Trace = [BoardIndex]

-- update the player turns based on index from 0 to the number - 1
turnBase :: Int -> PlayerIndex -> PlayerIndex
turnBase players idx = if idx == players - 1 then 0 else idx + 1
-- the operator of selected node record
push :: BoardIndex -> Trace -> Trace
push x xs = x:xs
pop :: Trace -> (BoardIndex, Trace)
pop xs = (last xs, init xs)

-- Normal MCTS is divided into four phases: selection, expansion, playout and backpropagation
-- the first phase is to select one resulting board with the maximum profit

-- the whole structure requires a state monad that stores the constructed game tree
-- the score is calculated based on different strategies: UCT for MCTS selection or move-based evaluation of minimax algorithm
searchNode :: BoardIndex -> GameTree -> GameTree
searchNode i t = if null $ searchNode' i t then error "Not exist" else head $ searchNode' i t
    where
        searchNode' :: BoardIndex -> GameTree -> [GameTree]
        searchNode' i t = if getBoardIndex t == i then [t]
                          else concatMap (searchNode' i) (getChildren t)

-- create a root node based on the number of players and main board
makeRoot :: Int -> Board -> SB BoardIndex GameTree
makeRoot players eboard = do idx <- stState
                             stUpdate (idx + 1) -- add an increment of the board index, such that it won't be duplicate  
                             return (GRoot idx eboard (replicate players 0) [])
-- create a leaf node from its parent internal node
makeLeaf :: Int -> (Board, Transform) -> SB BoardIndex GameTree
makeLeaf players (b, ft) = do idx <- stState
                              stUpdate (idx +1)
                              return (GLeaf idx b ft (replicate players 0))

-- generate a random value from 0 to 100, for certain percentage check
--randomPercentage :: Int -> BoardIndex -> Bool
randomPercentage :: Int -> Bool
randomPercentage n  = unsafePerformIO (randomRIO (0, 100)) <= n
-- generate a random index given a length
--randomMove :: Int -> BoardIndex -> Int
randomMove :: Int -> Int
randomMove l  = unsafePerformIO (randomRIO (0, l-1))
-- handle the situation where exists more than one maximum values
randomSelection :: [Wins] -> Int
randomSelection []  = error "Selection: no node for selecting"
randomSelection xs  = let is = elemIndices (maximum xs) xs
                      in  if length is == 1 then head is
                          else let ri = randomMove (length is)  -- random index of the maximum values' indices
                               in  is !! ri

-- prepend the root node to the trace 
-- select the nodes based on the strategy, and finally produce a list of traversed node
-- the move chocie could be varied, such as only choosing the non-backward moves
-- select the child nodes based on the most profits
selection :: PlayerIndex -> GameTree -> SB Trace (GameTree, PlayerIndex)
selection pi n = let ts = getChildren n
                 in  if null ts then return (n, pi) -- return the next node and player index for the expansion
                     else let pn = getPlayers n
                              wl = map (estimateNode pi) ts
                              sn = ts !! randomSelection wl  -- select the child with the maximum estimation 
                          in  do st <- stState
                                 stUpdate (push (getBoardIndex sn) st)      -- update the selection record
                                 selection (turnBase pn pi) sn  -- start the next selection at the selected node

-- expand a node based on the next possible move made by a certain player, and assign each board state an index
-- the expansion could apply various policies such as expanding multiple board states, or set a threshold of visits before any expansion
-- as well as the restriction of the expanded move's type, but the common one is to only expand once each time
expansion :: PlayerIndex -> GameTree -> SB BoardIndex GameTree
expansion pi n = let cs = getChildren n
                 in  if not (null cs) then return n -- no need to expand if already has children, mostly expanding for leaf
                 else do let pn = getPlayers n
                             co = currentPlayerColour pi pn
                             bs = expandingBoards co (getBoard n)
                         nc <- mapM (makeLeaf pn) (expandPolicy bs)
                         return (editNodeChildren nc n) -- add resulting boards as the leaf children nodes
    where
        expandPolicy bs = take 5 bs

-- start with updating the root's state
mainBackpropagation :: PlayerIndex -> Trace -> GameTree -> GameTree -> GameTree
mainBackpropagation pi xs r new = head (backpropagation pi xs [r] new)
-- update the game tree stored wins for each player, as well as replacing the new expanded node
backpropagation :: PlayerIndex -> Trace -> [GameTree] -> GameTree -> [GameTree]
backpropagation _ [] ts _ = ts
backpropagation pi xs ts new = let (bi, ys) = pop xs
                               in  case elemIndex bi (map getBoardIndex ts) of
                                        Nothing -> error "Trace incorrect"
                                        Just li -> let ln = ts !! li
                                                   in  if getBoardIndex ln == getBoardIndex new
                                                       then let n1 = editNodeValue pi new
                                                                n2 = backpropagation pi ys (getChildren n1) new
                                                                n3 = editNodeChildren n2 n1
                                                            in  replace li n3 ts
                                                       else let n1 = editNodeValue pi ln
                                                                n2 = backpropagation pi ys (getChildren n1) new
                                                                n3 = editNodeChildren n2 n1
                                                            in  replace li n3 ts

-- random greedy policy with certain precentage of choosing the best option while the remaining chance of random choice
-- get the best heuristic estimated board from all expanded boards 
playoutPolicy :: Colour -> Int -> [(Board, Transform)] -> (Board, Int)
playoutPolicy c s xs = let ps = map ((\(x, y) -> (projection c (getPos x), projection c (getPos y))) . snd) xs -- convert the positions for the boards
                           sl = map (flipBoardStateEvaluation s) ps -- calculate the heuristic scores for the possible expanded boards
                       in  if randomPercentage 95  then let idx = maxIndex sl -- get the best performed board
                                                       in  (fst (xs !! idx), sl !! idx)
                           else let idx = randomMove (length xs)  -- just choose a random board
                                in  (fst (xs !! idx), sl !! idx)
    where
        flipBoardStateEvaluation :: Int -> (Pos, Pos) -> Int
        flipBoardStateEvaluation c (f, t) = c - centroidPos f + centroidPos t
-- game simulation from a certain board state 
playout :: Board -> SB (PlayerIndex, Int) Board
playout b = do (pi, pn) <- stState
               let c = currentPlayerColour pi pn
                   s = centroid $ projectCOB c b
                   bs = expandingBoards c b
                   (nb, bv) = playoutPolicy c s bs
               if bv == 28 then return nb
               else do stUpdate (turnBase pn pi, pn)
                       playout nb

-- the MCTS structure that first selects the node with largest profits, then expands it, 
-- and play simulations on the expanded node, and finally update the reviewed nodes
mcts :: (GameTree, BoardIndex) -> Int -> (GameTree, BoardIndex)
mcts (tree, bi) pi = let ((lastNode, pi2), trace) = runState (selection pi tree) [getBoardIndex tree]
                         (expandedNode, bi2) = runState (expansion pi2 lastNode) bi -- ignore the threshold, just expand fully
                         ((playNode, pi3), trace1) = runState (selection pi2 expandedNode) trace -- select the best expanded child
                         (_, (winIdx, _)) = runState (playout (getBoard playNode)) (pi3, getPlayers playNode)
                         newGameTree = mainBackpropagation winIdx trace1 tree expandedNode -- update the wins for each traversed node
                     in  (newGameTree, bi2)
-- call multiple times of the four stages in order
iteration :: Int -> (GameTree, BoardIndex) -> PlayerIndex -> (GameTree, BoardIndex)
iteration 0 state _ = state
iteration counts state i = let newState = mcts state i
                           in iteration (counts - 1) newState i

-- ts = runState (makeRoot 3 (eraseBoard threePlayersSet externalBoard)) 0
finalSelection :: (GameTree, BoardIndex) -> PlayerIndex -> Board
finalSelection state playerIndex = let (tree, boardIdx) = iteration 3 state playerIndex
                                       sl = map ((!! playerIndex) . getWins) (getChildren tree)
                                   in  getBoard (getChildren tree !! maxIndex sl)


