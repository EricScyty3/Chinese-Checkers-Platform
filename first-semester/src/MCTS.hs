module MCTS where
-- the body of the Monte-Carlo Tree Search that contains: selection, expansion, playout, backpropagation, as well as final movement decision
import GameTree
import Board
import GHC.IO
import System.Random
import Zobrist
import Data.List
import Control.Monad.State
import Control.Parallel
import RBTree
import Data.Time
import BFS
import System.Environment

-- a list of node that is chosen along with the selection, used for updating the tree information as well as the history movements
type Trace = [BoardIndex]

-- the operator of selected node record
push :: BoardIndex -> Trace -> Trace
push x xs = x:xs
pop :: Trace -> (BoardIndex, Trace)
pop xs = (last xs, init xs)

-- create a initial node of the game tree: root, based on the number of players and board
-- and return the incremented board index for next node
makeRoot :: Int -> Board -> (GameTree, BoardIndex)
makeRoot players board = (GRoot 0 board (replicate players 0) [], 1) -- assign the board index, board configuration, players' wins, and empty children 
-- given a change of board, create a leaf node from a parent
makeLeaf :: Transform -> State GameTreeStatus GameTree
makeLeaf transform = do bi <- getBoardIdx
                        pn <- getPlayerNum
                        updateBoardIdx -- add an increment of the board index, such that it won't be duplicate  
                        return (GLeaf bi transform (replicate pn 0)) -- allocate the board index and no children as it's a leaf 

-- generate a random value from 0 to 100, for random percentage decision making
randomPercentage :: Int -> Bool
randomPercentage n  = unsafePerformIO (randomRIO (0, 100)) <= n
-- generate a random index with a given length, for random choice
randomMove :: Int -> Int
randomMove l  = unsafePerformIO (randomRIO (0, l-1))
-- selecting randomly if there exist multiple maximum elements in a list
randomSelection :: Ord a => [a] -> Int
randomSelection []  = error "Selection: no node for selecting"
randomSelection xs  = let is = elemIndices (maximum xs) xs
                      in  if length is == 1 then head is
                          else let ri = randomMove (length is)  -- random index of the maximum values' indices
                               in  is !! ri -- return the maximum value's index for selecting

-- get a list of non-repeated random values of a range
randomIndices :: Int -> [Int]
randomIndices l = unsafePerformIO $ take l . nub . randomRs (0, l-1) <$> newStdGen

--Stage Operators--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- normal MCTS is divided into four phases: selection, expansion, playout and backpropagation
-- the first phase is to select one resulting board with the maximum profit/score, which is calculated based on different formulas
-- select the nodes based on the maximum strategy from the root of the game tree to a leaf, and produce a list of traversed nodes
-- need to be noticed that the terminal node (the winning node) is also a leaf, therefore, terminate the selection when meeting a leaf is sufficient as long as it is always a leaf
selection :: GameTree -> Trace -> State GameTreeStatus (Trace, GameTree) -- the initial trace should already include the root node as it will always be selected
selection gametree trace = let childrenList = getChildren gametree
                           in  if null childrenList then return (trace, gametree)   -- return the node and trace for the next stage when meeting a leaf (the leaf might be a terminal point)
                               else do wl <- mapM (estimateNode (getVisits gametree)) childrenList -- produce a list of "scores" for all child nodes
                                       let sn = childrenList !! randomSelection wl  -- select the child with the maximum score 
                                           ntrace = push (getBoardIndex sn) trace   -- and update the trace
                                       nboard <- repaintBoard (getTransform sn)     -- update the board state by applying the pieces' change
                                       updatePlayerIdx      -- update the player index based on game turn, such that the each node estimation is for the current player
                                       updateBoard nboard
                                       selection sn ntrace  -- start the another selection next turn at the selected node

-- expansion: expand a node based on the next possible moves could be made by the current player, and assign each board with an index (make them leaves)
-- the expansion could apply various policies such as retricting the expanded nodes number, or only expand non-backward move, and so on
-- the expansion accept a tree node and return a node being expanded
expansion :: GameTree -> State GameTreeStatus GameTree
expansion n = let cs = getChildren n
              in  if not (null cs) then error "Can only expand not expanded node" -- return n -- no need to expand if already has children, mostly expanding for leaf
                  else do co <- getPlayerColour
                          ts <- colouredMovesList co
                          cs <- mapM makeLeaf (expandPolicy co ts) -- generate the leaf node for movement that is accepted for expanding
                          return (editNodeChildren cs n) -- the new resulting nodes will become the children of the expanded node 

-- the strategy of how a board could lead to different resulting boards
expandPolicy :: Colour -> [Transform] -> [Transform]
expandPolicy co xs
    | not $ null front = front  -- if front moves are avaliable then just expand them   
    | otherwise = nonfront      -- else, the non-frontward moves are accepted
    where
        -- the avaliable movements are divided into two categories: frontward, and non-frontward
        front = filter ((> 0) . distanceChange) xs
        nonfront = filter ((<= 0) . distanceChange) xs
        -- project the position from main board to the occupied board of certain colour
        distanceChange (x, y) = evaluateMove2 (projection co (getPos x), projection co (getPos y))

-- after the selection, expansion, and playout is done, a win is known from the game simulation in playout stage, and should be update to the selected nodes
-- update the game tree stored wins for certain player, as well as replacing the new expanded node to the game tree
-- the process containing traversing child nodes from the root until reaching the last node in the trace
backpropagation :: PlayerIndex -> Trace -> [GameTree] -> GameTree -> State GameTreeStatus [GameTree]
backpropagation _ [] cs _ = return cs
backpropagation pi xs cs new = let (bi, ys) = pop xs -- check the if any child node fit the search board index
                               in  case elemIndex bi (map getBoardIndex cs) of
                                            Nothing -> error "Trace incorrect"
                                            Just li -> let ln = cs !! li
                                                       in  do pi <- getPlayerIdx
                                                              pn <- getPlayerNum
                                                              ht <- getHistoryTrace
                                                              updateHistoryTrace (editHT ln pi pn ht) -- if found, the update the history first 
                                                              let n1 = (if getBoardIndex ln == getBoardIndex new -- then consider whether this is the node to be replaced by the expanded version
                                                                        then editNodeValue pi new
                                                                        else editNodeValue pi ln)
                                                              n2 <- backpropagation pi ys (getChildren n1) new -- recusivly searching down on the children 
                                                              let n3 = editNodeChildren n2 n1 -- finally update the children of this node
                                                              return (replace li n3 cs) -- a new list of updated child nodes is retrieved

--Simulation-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- how the game simulation is done during the playout stage

-- different approaches for estimating the current board state during the playout stage
-- 1. movement distance: move evaluation
-- 2. lookup table search: board evaluation

-- simple move evalutor of how close the changed piece is to the goal state, 
-- using centroid function in BFS, that the closer it is, the larger it will be
evaluateMove :: (Pos, Pos) -> Int
evaluateMove (p1, p2) = let cen1 = centroidPos p1
                            cen2 = centroidPos p2
                        in  cen1 `par` cen2 `pseq` (cen2 - cen1) -- the larger, the closer the new position is to the goal state

-- an addition move evaluator is to measure the forward distance to the goal base
dist :: Pos -> Pos -> Double
dist (x1, y1) (x2, y2) = sqrt (fromIntegral (x1 - x2)^2 + fromIntegral (y1 - y2)^2)
-- return the distance of a piece to the goal state, the evaluation is more straightforward as only considering the distance
evaluateMove2 :: (Pos, Pos) -> Double
evaluateMove2 (p1, p2) = let dist1 = dist p1 (0, 6)
                             dist2 = dist p2 (0, 6)
                         in dist1 `par` dist2 `pseq` (dist1 - dist2) -- still the larger the better

-- random greedy policy with certain precentage of choosing the best option while the remaining chance of random choice if applied here
playoutPolicy :: Colour -> [Transform] -> State GameTreeStatus Board
playoutPolicy colour tfs = let ptfs = map (\(x, y) -> (projection colour (getPos x), projection colour (getPos y))) tfs -- projected to corresponding occupied board
                               sl   = map evaluateMove2 ptfs -- measure change of distance made by the movements 
                           in  if randomPercentage 95 then do let cidx = randomSelection sl
                                                                  cft  = tfs !! cidx
                                                              repaintBoard cft -- get the board that lead to largest distance increment 
                               else do let cidx = randomMove (length tfs) -- 5% of chance for randomly play a move
                                           cft  = tfs !! cidx
                                       repaintBoard cft

-- game simulation from a certain board state 
playout :: Int -> State GameTreeStatus (PlayerIndex, Int)
playout moves = if moves >= 1000 then error "too many loads"
                else
                do colour <- getPlayerColour
                   tfs <- colouredMovesList colour    -- get all of the avaliable moves
                   board <- getBoard
                   if winStateDetermine colour board && moves == 1 then error "Cannot start playout at terminal point" -- if the started board state is already an end state then do nothing
                   else {-if winStateDetermine colour board && moves > 1 then error ("Error: " ++ show pi ++ show moves ++ show board)
                        else -}
                        do nboard <- playoutPolicy colour tfs -- otherwise, choose one of the boards resulted from the current board
                           pi <- getPlayerIdx
                           pn <- getPlayerNum
                           if winStateDetermine colour nboard then return (pi, getTurns moves pn) -- if a player wins, then return the player's index
                           else do updatePlayerIdx -- otherwise, keep simulating on the next turn
                                   updateBoard nboard
                                   playout (moves + 1)

getTurns :: Int -> Int -> Int
getTurns moves pn = ceiling (fromIntegral moves / fromIntegral pn)

-- the win state detection here is not just checking the hash state, should consider not only the normal wining state, but also the potential blocking
-- a state is won for a player if its goal area is filled with pieces, and at least one of the pieces belongs it
winStateDetermine :: Colour -> Board -> Bool
winStateDetermine c b = let hs = map (reversion c) goalBase -- get the goal positions of certain player
                            bs = evalState (do mapM getElement hs) b -- get the corresponding board state
                        in  isFull bs && existColour c bs -- check if the two conditions are satisfied
    where
        isFull :: [BoardPos] -> Bool
        isFull = foldr ((&&) . isOccupied) True
        existColour :: Colour -> [BoardPos] -> Bool
        existColour c bs = Just c `elem` map Board.getColour bs

        standardWinState :: Colour -> [BoardPos] -> Bool
        standardWinState c bs = let ps = map (projection c . getPos) bs
                                in  winStateDetectHash (hash ps)

--MCTS Body--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- the MCTS structure that first selects the node with largest profits, then expands it, 
-- and play simulations on the expanded node, and finally update the reviewed nodes
mcts :: GameTree -> State GameTreeStatus (GameTree, BoardIndex, HistoryTrace, Int)
mcts tree = do (trace, lastnode) <- selection tree [getBoardIndex tree] -- given a trace where the root is already inside it
               -- a check is taken to ensure whether the last node is a win state
               board <- getBoard
               pi <- getPlayerIdx
               pn <- getPlayerNum
               co <- getPlayerColour
               let ri = reverseTurnBase pn pi
                   rc = playerColour ri pn
               -- the reason why this check is necessary it that as the tree gradually grows, it will eventually reach the goal state, therefore, 
               -- need to determine them in advance, otherwise, playout might be mislead 
               if winStateDetermine rc board then do newTree <- backpropagation ri trace [tree] lastnode -- skip the expansion and playout phases if already won
                                                     bi <- getBoardIdx
                                                     ht <- getHistoryTrace
                                                     return (head newTree, bi, ht, 0) -- get the new tree and game history with playout turn equals to 0 
               else do expandednode <- expansion lastnode -- expand the last leaf in the trace
                       (ntrace, playnode) <- selection expandednode trace -- additional selection of the expanded node's children
                       board <- getBoard
                       if winStateDetermine co board then do newTree <- backpropagation pi ntrace [tree] expandednode -- skip the playout phases if already won
                                                             bi <- getBoardIdx
                                                             ht <- getHistoryTrace
                                                             return (head newTree, bi, ht, 0) -- get the new tree and game history with playout turn equals to 0 
                       else do (winIdx, turns) <- playout 1 -- the standard order: the playout will be done based on the board of the selected new leaf
                               newTree <- backpropagation winIdx ntrace [tree] expandednode -- treat the tree as a child by setting it into a list, and update it
                               bi <- getBoardIdx
                               ht <- getHistoryTrace
                               return (head newTree, bi, ht, turns) -- get the new tree 

-- repeating the MCTS until certain iterations are reached
iterations :: GameTree -> GameTreeStatus -> [Int] -> Int -> (GameTree, BoardIndex, HistoryTrace, [Int])
iterations tree s@(_, bi, _, _, ht, _) ts 0 = (tree, bi, ht, reverse ts)
iterations tree s@(pi, bi, board, pn, ht, cons) ts c = let (newTree, nbi, nht, turns) = evalState (mcts tree) s -- reset every status while maintaining the board index and move history
                                                       in  iterations newTree (pi, nbi, board, pn, nht, cons) (turns:ts) (c-1) -- inherit the movement history, and record the playout turns

-- after the iterations of four stages are finished running, the root shoudl choose the child with the most win rate for the next move 
-- the tree could be re-used for saving computation, but if looking for different result, the tree should be started from scratch every several iterations
finalSelection :: GameTree -> GameTreeStatus -> Int -> Int -> (Board, Int, HistoryTrace, [Int])
finalSelection tree s@(pi, _, board, pn, _, _) bhash counts = let (ntree, _, nht, pl) = iterations tree s [] counts
                                                                  scores = map (averageScore pi) (getChildren ntree) -- list all win rate for a player
                                                              in  if null scores then error (show ntree)
                                                                  else let idx = randomSelection scores
                                                                           chosenNode = getChildren ntree !! idx -- get the maximum win rate move as the next movement
                                                                           colour = playerColour pi pn
                                                                           (from, to) = getTransform chosenNode
                                                                           newBoard = evalState (repaintBoard (from, to)) s
                                                                           newBoardHash = changeHash (projection colour (getPos from)) (projection colour (getPos to)) bhash
                                                                       in  newBoard `par` newBoardHash `pseq` (newBoard, newBoardHash, nht, pl)
                                                                      -- depending on the need, several information could be returned 
                                                                      -- the new board state and the new board hash are generated for Computer decision in Main
                                                                      -- while playouts are treated as one of the measurements in experiments
                                                                      -- the game history is maintained globally for the next call