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
import Configuration
import Minimax


-- a list of node that is chosen along with the selection, used for updating the tree information as well as the history movements

-- the operator of selected node record, first-in-first-out
push :: a -> [a] -> [a]
push x xs = x:xs
pop :: [a] -> (a, [a])
pop xs = (last xs, init xs)

-- get project all pieces from the external board to the internal version, and transform them into a list of positions
-- initialInternalBoard :: Board -> Int -> [[Pos]]
-- initialInternalBoard eboard pn = map (convertToInternalBoard eboard) (playerColourList pn)
-- given a change of board, create a leaf node from a parent
makeLeaf :: Transform -> State GameTreeStatus GameTree
makeLeaf transform = do bi <- getBoardIdx
                        pn <- getPlayerNum
                        bi `par` pn `pseq` updateBoardIdx -- add an increment of the board index, such that it won't be duplicate  
                        return (GLeaf bi transform (replicate pn 0)) -- allocate the board index and no children as it's a leaf 

-- generate a random value from 0 to 100, for random percentage decision making
randomPercentage :: Int -> Bool
randomPercentage n  = unsafePerformIO (randomRIO (0, 100)) <= n
-- generate a random index with a given length, for random choice
randomMove :: Int -> Int
randomMove l  = unsafePerformIO (randomRIO (0, l-1))
-- selecting randomly if there exist multiple maximum elements in a list
randomMaxSelection :: Ord a => [a] -> Int
randomMaxSelection []  = error "Selection: no node for selecting"
randomMaxSelection xs  = let is = elemIndices (maximum xs) xs
                             ri = is `seq` randomMove (length is)  -- random index of the maximum values' indices
                         in  is !! ri -- return the maximum value's index for selecting
-- get a list of non-repeated random values of a range
randomIndices :: Int -> [Int]
randomIndices l = unsafePerformIO $ take l . nub . randomRs (0, l-1) <$> newStdGen

--Stage Operators--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- normal MCTS is divided into four phases: selection, expansion, playout and backpropagation

-- the first phase is to select one resulting board with the maximum profit/score, which is calculated based on different formulas
-- select the nodes based on the maximum strategy from the root of the game tree to a leaf, and produce a list of traversed nodes (indices) and the corresponding board hash value
-- need to be noticed that the terminal node (the winning node) is also a leaf, therefore, terminate the selection when meeting a leaf is sufficient as long as it is always a leaf

selection :: GameTree -> [Int] -> [Int] -> State GameTreeStatus ([Int], [Int], GameTree)
selection gametree indexList hashList = let childrenList = getChildren gametree
                                        in  if null childrenList then return (indexList, hashList, gametree)   -- return the node and trace for the next stage when meeting a leaf (the leaf might be a terminal point)
                                            else do scoreList <- mapM (estimateNode (getVisits gametree)) childrenList -- produce a list of "scores" for all child nodes
                                                    let selectedNode = childrenList !! randomMaxSelection scoreList  -- select the child with the maximum score 
                                                        movement = getTransform selectedNode
                                                    -- update the internal board for the current player
                                                    nodeInternalState <- modifyCurrentInternalBoard movement
                                                    updateCurrentInternalBoard nodeInternalState
                                                    -- record the selected node's state for later backpropagation 
                                                    -- how the path is digging down and recording the node's board state
                                                    let newindexList = push (getBoardIndex selectedNode) indexList
                                                        newhashList = push (hash nodeInternalState) hashList
                                                    -- update the external board
                                                    newboard <- repaintBoard movement
                                                    setBoard newboard
                                                    -- update the player's turn
                                                    updatePlayerIdx
                                                    -- start the selection at the next player's turn
                                                    newindexList `par` newhashList `pseq` selection selectedNode newindexList newhashList

-- expansion: expand a node based on the next possible moves could be made by the current player, and assign each board with an index (make them leaves)
-- the expansion could apply various policies such as retricting the expanded nodes number, or only expand non-backward move, and so on
-- the expansion accept a tree node and return a node being expanded
expansion :: GameTree -> State GameTreeStatus GameTree
expansion n = let cs = getChildren n
              in  if not (null cs) then error "Can only expand not expanded node" -- return n -- no need to expand if already has children, mostly expanding for leaf
                  else do co <- getPlayerColour
                          pi <- getPlayerIdx
                          ts <- colouredMovesList pi
                          cs <- co `par` ts `pseq` mapM makeLeaf (expandPolicy co ts) -- generate the leaf node for movement that is accepted for expanding
                          return (editNodeChildren cs n) -- the new resulting nodes will become the children of the expanded node 

-- the strategy of how a board could lead to different resulting boards
expandPolicy :: Colour -> [Transform] -> [Transform]
expandPolicy co xs
    | not $ null front = front  -- if front moves are avaliable then just expand them   
    | otherwise = xs            -- else, all moves are accepted
    where
        -- the avaliable movements are divided into two categories: frontward, and non-frontward
        front = filter ((> 0) . distanceChange) xs
        -- project the position from main board to the occupied board of certain colour
        distanceChange tf = moveEvaluation (projectMove co tf)

-- after the selection, expansion, and playout is done, a win is known from the game simulation in playout stage, and should be update to the selected nodes
-- update the game tree stored wins for certain player, as well as replacing the new expanded node to the game tree
-- the process containing traversing child nodes from the root until reaching the last node in the trace

-- fixing board update issues: now holding the internal board state rather than just two-position change
-- after the selection and playout, the game status is changed, and to update the search tree
backpropagation :: PlayerIndex -> [Int] -> GameTree -> GameTree -> State GameTreeStatus GameTree
backpropagation _ [] node _ = return node
backpropagation winIdx itrace node newnode = let (idx, rtrace) = pop itrace
                                                 tempNode = (if getBoardIndex node == getBoardIndex newnode then newnode else node)
                                                 children = getChildren tempNode
                                             in  case elemIndex idx (map getBoardIndex children) of
                                                    Nothing -> error "Trace incorrect"
                                                    Just si -> do let sn = children !! si -- find the selected node in the selection phase
                                                                      -- then consider whether this is the node to be replaced by the expanded one
                                                                      -- and update the wins by incrementing by 1
                                                                      n1 = editNodeValue winIdx sn
                                                                  -- recusivly searching down on the children 
                                                                  n2 <- backpropagation winIdx rtrace n1 newnode
                                                                  -- finally update the children of this node
                                                                  let newChildren = replace si n2 children
                                                                  -- a new list of updated child nodes is retrieved for the entered node
                                                                  return (editNodeChildren newChildren node)
-- since the history trace should be updated as well every time a selection is made
editHT :: [Int] -> PlayerIndex -> HistoryTrace -> State GameTreeStatus HistoryTrace
editHT [] _ ht = return ht -- finally return the edited history trace
editHT (h:hs) winIdx ht = do pn <- getPlayerNum
                             let nht = (case rbSearch h ht of
                                            -- if not then add one
                                            Nothing -> rbInsert h (initWins pn) ht
                                            -- otherwise, update the wins
                                            Just ws -> rbInsert h (replace winIdx ((ws!!winIdx)+1) ws) ht)
                             -- keep processing on the rest of the history trace
                             editHT hs winIdx nht
    where
        initWins pn = replace winIdx 1 (replicate pn 0)

--Simulation-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- how the game simulation is done during the playout stage

-- different approaches for estimating the current board state during the playout stage
-- 1. movement distance: move evaluation
-- 2. lookup table search: board evaluation
-- 3. shallow search equipped with board evaluation

-- random greedy policy with certain precentage of choosing the best option while the remaining chance of random choice if applied here
switchEvaluator :: PlayoutArgument -> [Transform] -> State GameTreeStatus Transform
switchEvaluator (evaluator, depth, killerMoves) tfs =
                                                if not (randomPercentage 95) || evaluator == RandomEvaluator
                                                then do let idx = randomMove (length tfs)
                                                        return (tfs !! idx) -- randomly choose a movement and generate the resulting board  
                                                else case evaluator of
                                                        -- move evaluator
                                                        MoveEvaluator -> moveEvaluator tfs
                                                        -- board evaluator
                                                        BoardEvaluator -> boardEvaluator tfs
                                                        MixedParanoid -> mixedSearch Paranoid depth killerMoves tfs
                                                        MixedBRS -> mixedSearch BRS depth killerMoves tfs
                                                        _ -> error "Undefined Evaluator"
    where
        moveEvaluator :: [Transform] -> State GameTreeStatus Transform
        moveEvaluator tfs = do colour <- getPlayerColour
                               let ptfs = map (projectMove colour) tfs
                                   scoreList = map moveEvaluation ptfs
                                   idx = randomMaxSelection scoreList
                               return (tfs !! idx)

        boardEvaluator :: [Transform] -> State GameTreeStatus Transform
        boardEvaluator tfs = do psList <- mapM modifyCurrentInternalBoard tfs
                                let scoreList = boardEvaluations psList
                                    idx = randomMaxSelection scoreList
                                return (tfs !! idx)

        minimaxSearch :: Int -> [KillerMoves] -> TreeType -> State GameTreeStatus Transform
        minimaxSearch 0 _ _ = error "the depth should be larger than zero"
        minimaxSearch depth kms treetype = do (ri, _, eboard, iboards, pn, _, _, _) <- get
                                              let ((move, _), nkms) = runState (mEvaluation depth (ri, eboard, iboards, pn, (-999, 999), treetype) ri) kms
                                              updatePlayoutKillerMoves nkms
                                              return move

        mixedSearch :: TreeType -> Int -> [KillerMoves] -> [Transform] -> State GameTreeStatus Transform
        mixedSearch treetype depth kms ms = do iboard <- getCurrentInternalBoard
                                               if isMidgame iboard then minimaxSearch depth kms treetype
                                               else boardEvaluator ms

-- game simulation from a certain board state 
playout :: Int -> State GameTreeStatus (PlayerIndex, Int)
playout moves =
                do pn <- getPlayerNum
                   if getTurns moves pn >= 1000 then do -- avoid the potential cycling, or stop the playouts if costing too much time
                                                        iboards <- getInternalBoard
                                                        let scoreList = boardEvaluations iboards
                                                        -- treat the one with the best board state (not move state) as winner
                                                        return (randomMaxSelection scoreList, getTurns moves pn)
                                                        -- error "Exceed 1000 turns"
                   else do pi <- getPlayerIdx
                           tfs <- colouredMovesList pi    -- get all of the avaliable moves (might cause cycling)
                           board <- getBoard
                           pa <- getPlayoutArgument
                           colour <- getPlayerColour
                           if colour `par` board `pseq` winStateDetermine colour board 
                              && moves == 1 then error ("Cannot start playout at terminal point:" ++ show pi ++ "\n" ++ show board)
                            -- if the started board state is already an end state, this means that some players take a suicidal action that cause other player to win
                           else do resultedMove <- switchEvaluator pa tfs -- otherwise, choose one of the boards resulted from the current board   
                                   -- update internal board state
                                   -- let checkedMove = (if resultedMove `elem` record then tfs !! randomMove (length tfs) else resultedMove)
                                   newps <- modifyCurrentInternalBoard resultedMove
                                   updateCurrentInternalBoard newps
                                   -- reflect the change onto external board
                                   nboard <- repaintBoard resultedMove
                                   if winStateDetermine colour nboard then return (pi, getTurns moves pn) -- if a player wins, then return the player's index
                                   else do updatePlayerIdx -- otherwise, keep simulating on the next turn
                                           setBoard nboard
                                           playout (moves + 1) -- record the recently made moves

getTurns :: Int -> Int -> Int
getTurns moves pn = ceiling (fromIntegral moves / fromIntegral pn)

--MCTS Body--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- the MCTS structure that first selects the node with largest profits, then expands it, 
-- and play simulations on the expanded node, and finally update the reviewed nodes

mcts :: GameTree -> State GameTreeStatus (GameTree, BoardIndex, HistoryTrace, Int, [KillerMoves])
mcts tree = do (idxList, hashList, lastnode) <- selection tree [] []
               -- a check is taken to ensure whether the last node is a win state, this allowing the suicidal action that cause other player to win
               -- therefore, the check should consider all players
               pn <- getPlayerNum
               board <- getBoard
               let winIdx = pn `par` board `pseq` checkPlayersWinState pn board
               -- the reason why this check is necessary it that as the tree gradually grows, it will eventually reach the goal state, therefore, 
               -- need to determine them in advance, otherwise, playout might be misled 
               if winIdx /= -1 then do newTree <- backpropagation winIdx idxList tree lastnode -- skip the expansion and playout phases if already won
                                       ht <- getHistoryTrace
                                       newHistory <- editHT hashList winIdx ht
                                       bi <- getBoardIdx
                                       (_, _, kms) <- getPlayoutArgument
                                       return $ newTree `par` newHistory `pseq` (newTree, bi, newHistory, 0, kms) -- get the new tree and game history with playout turn equals to 0 

               else do expandednode <- expansion lastnode -- expand the last leaf in the trace
                       (idxList2, hashList2, _) <- selection expandednode idxList hashList -- additional selection of the expanded node's children
                       board2 <- getBoard
                       let winIdx = checkPlayersWinState pn board2
                       if winIdx /= -1 then do newTree2 <- backpropagation winIdx idxList2 tree expandednode -- skip the playout phases if already won
                                               bi <- getBoardIdx
                                               ht <- getHistoryTrace
                                               newHistory2 <- editHT hashList2 winIdx ht
                                               (_, _, kms) <- getPlayoutArgument
                                               return $ newTree2 `par` newHistory2 `pseq` (newTree2, bi, newHistory2, 0, kms) -- get the new tree and game history with playout turn equals to 0 

                       else do (winIdx, turns) <- playout 1 -- start the playout phase with certain policy and heuristic
                               -- current killer moves and moves record are reset everytime entering the playout phase
                               newTree3 <- backpropagation winIdx idxList2 tree expandednode -- treat the tree as a child by setting it into a list, and update it
                               bi <- getBoardIdx
                               ht <- getHistoryTrace
                               newHistory3 <- editHT hashList2 winIdx ht
                               (_, _, kms) <- getPlayoutArgument
                               return $ newTree3 `par` newHistory3 `pseq`(newTree3, bi, newHistory3, turns, kms) -- get the new tree 

-- check the win state of all players, since the suicidal action is allowed and can be performed either randomly or on purpose
-- any value other than -1 means a win is reached, otherwise, nothing
checkPlayersWinState :: Int -> Board -> Int
checkPlayersWinState pn board = let ws = map (`winStateDetermine` board) (playerColourList pn)
                                in  case elemIndices True ws of
                                        [] -> -1
                                        [x] -> x
                                        _ -> error "Multiple players win at the same time"

-- repeating the MCTS until certain iterations are reached
iterations :: GameTree -> GameTreeStatus -> [Int] -> Int -> (GameTree, BoardIndex, HistoryTrace, [Int])
iterations tree s@(_, bi, _, _, _, ht, _, _) playoutTurns 0 = (tree, bi, ht, reverse playoutTurns)
iterations tree s@(pi, bi, board, ps, pn, ht, cons, (eval, depth, _)) playoutTurns count = 
    let (newTree, newIdx, newHistory, turns, kms) = evalState (mcts tree) s -- reset every status while maintaining the board index and move history
    in  iterations newTree (pi, newIdx, board, ps, pn, newHistory, cons, (eval, depth, kms)) (turns:playoutTurns) (count-1) -- inherit the movement history, and record the playout turns




