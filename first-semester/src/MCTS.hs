module MCTS where

import GameTree
import State
import ShortestPath
import Board
import GHC.IO
import System.Random
import Zobrist
import Data.List (elemIndex)

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

-- determine a node's profits based on how well it wins
estimateNode :: PlayerIndex -> GameTree -> Double
estimateNode i t = if getVisits t == 0 then 0
                   else fromIntegral (getWins t !! i) / fromIntegral (getVisits t)

-- prepend the root node to the trace 
-- select the nodes based on the strategy, and finally produce a list of traversed node
-- the move chocie could be varied, such as only choosing the non-backward moves
-- select the child nodes based on the most profits
selection :: PlayerIndex -> GameTree -> SB Trace (GameTree, PlayerIndex)
selection pi n = let ts = getChildren n
                 in  if null ts then return (n, pi) -- return the next node and player index for the expansion
                     else let pn = getPlayers n
                              sl = map (estimateNode pi) ts
                              sn = ts !! maxIndex sl -- select the child with the maximum estimation 
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
                         return (editNodeChildren nc n) -- add all resulting boards as the leaf children node
    where
        expandPolicy bs = bs

-- start with updating the root's state
mainBackpropagation :: PlayerIndex -> Trace -> GameTree -> GameTree -> GameTree
mainBackpropagation pi xs r new = let newRoot = editNodeValue pi r
                                  in  backpropagation pi xs newRoot new
-- update the game tree stored wins for each player, as well as replacing the new expanded node
backpropagation :: PlayerIndex -> Trace -> GameTree -> GameTree -> GameTree
backpropagation _ [] t _ = t
backpropagation pi xs t new = let (idx, ys) = pop xs
                                  cs = getChildren t
                              in  case findChildNode idx t of
                                        Nothing -> error "Trace incorrect"
                                        Just ci -> let cn = cs !! ci
                                                   in  if getBoardIndex cn == getBoardIndex new
                                                       then let n1 = backpropagation pi ys (editNodeValue pi new) new
                                                            in  editNodeChildren (replace ci n1 cs) t
                                                       else let n2 = backpropagation pi ys (editNodeValue pi cn) new
                                                            in  editNodeChildren (replace ci n2 cs) t

-- generate a random value from 0 to 100, for certain percentage check
randomPercentage :: Int -> Bool
randomPercentage n = unsafePerformIO (randomRIO (0, 100)) <= n
-- generate a random index given a length
randomMove :: Int -> Int
randomMove l = unsafePerformIO (randomRIO (0, l-1))
-- random greedy policy with certain precentage of choosing the best option while the remaining chance of random choice
randomPolicy :: Colour -> Int -> [(Board, Transform)] -> Board
randomPolicy c s xs = if randomPercentage 95 then bestBoard c s xs else fst (xs !! randomMove (length xs)) -- either return the best board or randomly
-- get the best heuristic estimated board from all expanded boards 
bestBoard :: Colour -> Int -> [(Board, Transform)] -> Board
bestBoard c s xs = let ps = map ((\(x, y) -> (projection c (getPos x), projection c (getPos y))) . snd) xs -- convert the positions for the boards
                       sl = map (flipBoardState s) ps -- calculate the heuristic scores for the possible expanded boards
                   in  fst (xs !! maxIndex sl) -- return the maximum board
    where
        flipBoardState :: Int -> (Pos, Pos) -> Int
        flipBoardState c (f, t) = c - centroidPos f + centroidPos t

playout :: Board -> Int -> SB PlayerIndex Board
playout b p = do idx <- stState
                 let c = currentPlayerColour idx p
                     s = centroid $ projectCOB c b
                 if  s == 28 then return b
                 else do let bs = expandingBoards c b
                             nb = randomPolicy c s bs
                         stUpdate (turnBase p idx)
                         playout nb p

myTree :: Int -> SB BoardIndex GameTree
myTree p = do root <- makeRoot p testBoard
              newRoot <- expansion 0 root
              let c:cs = getChildren newRoot
              newChild <- expansion 1 c
              return (editNodeChildren (newChild:cs) newRoot)
    where
        testBoard
            | p == 2 = eraseBoard twoPlayersSet externalBoard
            | p == 3 = eraseBoard threePlayersSet externalBoard
            | p == 4 = eraseBoard fourPlayersSet externalBoard
            | otherwise = externalBoard

-- the MCTS structure that first selects the node with largest profits, then expands it, 
-- and play simulations on the expanded node, and finally update the reviewed nodes
mcts :: (GameTree, BoardIndex) -> Int -> (GameTree, BoardIndex)
mcts (tree, bi) pi = let ((lastNode, pi2), trace) = runState (selection pi tree) []
                         (expandedNode, bi2) = runState (expansion pi2 lastNode) bi -- ignore the threshold, just expand fully
                         ((playNode, pi3), trace1) = runState (selection pi2 expandedNode) trace -- select the best expanded child
                         (_, winIdx) = runState (playout (getBoard playNode) (getPlayers playNode)) pi3
                         newGameTree = mainBackpropagation winIdx trace1 tree expandedNode -- update the wins for each traversed node-}
                     in  (newGameTree, bi2)

iteration :: Int -> (GameTree, BoardIndex) -> PlayerIndex -> GameTree
iteration 0 (t, i)_  = t
iteration counts state playerIndex = let newState = mcts state playerIndex
                                     in  iteration (counts - 1) newState playerIndex

finalSelection :: (GameTree, BoardIndex) -> PlayerIndex -> GameTree
finalSelection state playerIndex = let tree = iteration 100 state playerIndex
                                       sl = map ((!! playerIndex) . getWins) (getChildren tree)
                                   in  getChildren tree !! maxIndex sl