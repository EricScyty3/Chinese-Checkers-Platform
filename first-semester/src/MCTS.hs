module MCTS where

import GameTree
import State
import ShortestPath
import Board
import GHC.IO
import System.Random
import Zobrist

type Trace = [BoardIndex]

testBoard :: Board
testBoard = eraseBoard False threePlayersSet externalBoard

turnBase :: Int -> PlayerIndex -> PlayerIndex
turnBase player idx = if idx == player - 1 then 0 else idx + 1

-- Normal MCTS is divided into four phases: selection, expansion, playout and backpropagation
-- the first phase is to select one resulting board with the maximum profit

-- the whole structure requires a state monad that stores the constructed game tree
-- the score is calculated based on different strategies: UCT for MCTS selection or move-based evaluation of minimax algorithm

searchNode :: BoardIndex -> [GameTree] -> GameTree
searchNode _ [] = error "No such board index"
searchNode idx ts = let is = map getBoardIndex ts
                    in  if idx `elem` is then let (node, _) = findChildNode idx ts
                                              in   node
                        else searchNode idx (concatMap getChildren ts)

-- create the initial tree with current board state
-- runState (makeRoot 2 externalBoard) 0
makeRoot :: Int -> Board -> SB BoardIndex GameTree
makeRoot players eboard = do idx <- stState
                             stUpdate (idx + 1)
                             return (GRoot idx eboard (replicate players 0) [])

makeLeaf :: Int -> (Board, Transform) -> SB BoardIndex GameTree
makeLeaf players (b, ft) = do idx <- stState
                              stUpdate (idx +1)
                              return (GLeaf idx b ft (replicate players 0))

-- estimateNode :: PlayerIndex -> GameTree -> Int
-- estimateNode p t = let b = getBoard t
--                        c = currentPlayerColour p (getPlayers t)
--                        ob = projectCOB c b
--                    in  centroid ob

estimateChildren :: PlayerIndex -> GameTree -> [Int]
estimateChildren p t = let cs = getChildren t
                       in  if null cs then []
                           else let c = currentPlayerColour p (getPlayers t)
                                    b = projectCOB c (getBoard t) -- the occupied board for the parent node
                                    s = centroid b
                                    ps = map ((\(x, y) -> (projection c (getPos x), projection c (getPos y))) . getTransform) cs
                                in  map (flipBoardState s) ps
    where
        flipBoardState :: Int -> (Pos, Pos) -> Int
        flipBoardState c (f, t) = c - centroidPos f + centroidPos t

myTree :: Int -> SB BoardIndex GameTree
myTree p = do root <- makeRoot p externalBoard
              newRoot <- expansion 0 root
              let c:cs = getChildren newRoot
              newChild <- expansion 1 c
              return (editNodeChildren (newChild:cs) newRoot)

myTest :: (GameTree, BoardIndex) -> Int -> (GameTree, Trace)
myTest (tree, bi) pi = let ((lastNode, npi), trace) = runState (selection pi tree) [getBoardIndex tree] -- start with trace containing the root
                           (expandedNode, nbi) = runState (expansion npi lastNode) bi -- ignore the threshold, just expand fully
                           (_, trace1) = runState (selection npi expandedNode) trace -- select the best expanded child
                           winIdx = 0 -- the winning player
                           newGameTree = backpropagation winIdx trace1 [tree] expandedNode -- update the wins for each traversed node-}
                       in  (head newGameTree, trace1)


push :: BoardIndex -> Trace -> Trace
push x xs = x:xs

pop :: Trace -> (BoardIndex, Trace)
pop xs = (last xs, init xs)

-- prepend the root node to the trace 
-- select the nodes based on the strategy, and finally produce a list of traversed node
-- the move chocie could be varied, such as only choosing the non-backward moves
selection :: PlayerIndex -> GameTree -> SB Trace (GameTree, PlayerIndex)
selection pi n = let ts = getChildren n
                 in  if null ts then return (n, pi) -- return the next node and player index for the expansion
                     else let sl = estimateChildren pi n -- map (estimateNode pi) ts
                              sn = ts !! maxIndex sl
                          in  do st <- stState
                                 stUpdate (push (getBoardIndex sn) st)
                                 selection (turnBase (getPlayers n) pi) sn

-- expand a node based on the next possible move made by a certain player, and assign each board state an index
-- the expansion could apply various policies such as expanding multiple board states, or set a threshold of visits before any expansion
-- as well as the restriction of the expanded move's type, but the common one is to only expand once each time
expansion :: PlayerIndex -> GameTree -> SB BoardIndex GameTree
expansion pi n = let cs = getChildren n
                 in  if not (null cs) then return n -- no need to expand if already has children 
                 else do let pn = getPlayers n
                             co = currentPlayerColour pi pn
                             bs = expandingBoards co (getBoard n)
                         nc <- mapM (makeLeaf pn) bs -- fully expand
                         return (editNodeChildren nc n)
    where
        expansionPolicy :: PlayerIndex -> Int -> [(Board, Transform)] -> [(Board, Transform)]
        expansionPolicy pi pn bs = [head bs] {-let sl = map (estimateBoard pi pn . fst) bs
                                   in  bs !! maxIndex sl-}
-- after expansion, the new node might be added to the tree as well as the trace 

-- the problem is how to replace the new expanded node to its parent node:
-- could be done when backpropagation, while updating the wins, also replace the last node with the expanded version

-- update the game tree stored wins for each player
backpropagation :: PlayerIndex -> Trace -> [GameTree] -> GameTree -> [GameTree]
backpropagation _ [] _ _ = []
backpropagation pi xs ts new = let (boardIdx, ys) = pop xs
                                   (n, idx) = findChildNode boardIdx ts
                                   in  if getBoardIndex n == getBoardIndex new then let n1 = editNodeValue pi new
                                                                                        nc = backpropagation pi ys (getChildren n1) new
                                                                                        n2 = editNodeChildren nc n1
                                                                                    in  replace idx n2 ts
                                       else let n1 = editNodeValue pi n
                                                nc = backpropagation pi ys (getChildren n1) new
                                                n2 = editNodeChildren nc n1
                                            in  replace idx n2 ts

-- generate a random value from 0 to 100, for certain percentage check
randomPercentage :: Int -> Bool
randomPercentage n = unsafePerformIO (randomRIO (0, 100)) <= n

randomMove :: Int -> Int
randomMove l = unsafePerformIO (randomRIO (0, l-1))

playout :: Board -> Int -> SB PlayerIndex Board
playout b p = do idx <- stState
                 let c = currentPlayerColour idx p
                     s = centroid $ projectCOB c b
                 if  s == 28 then return b
                 else do let bs = expandingBoards c b
                             nb = randomPolicy c s bs
                         stUpdate (turnBase p idx)
                         playout nb p

randomPolicy :: Colour -> Int -> [(Board, Transform)] -> Board
randomPolicy c s xs = if randomPercentage 95 then bestBoard c s xs else fst (xs !! randomMove (length xs)) -- either return the best board or randomly

bestBoard :: Colour -> Int -> [(Board, Transform)] -> Board
bestBoard c s xs = let ps = map ((\(x, y) -> (projection c (getPos x), projection c (getPos y))) . snd) xs -- convert the positions for the boards
                       sl = map (flipBoardState s) ps -- calculate the heuristic scores for the possible expanded boards
                   in  fst (xs !! maxIndex sl) -- return the maximum board
    where
        flipBoardState :: Int -> (Pos, Pos) -> Int
        flipBoardState c (f, t) = c - centroidPos f + centroidPos t