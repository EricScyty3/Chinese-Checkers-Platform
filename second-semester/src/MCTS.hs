module MCTS where
-- the four phases of the Monte-Carlo Tree Search: selection, expansion, playout, backpropagation, and their policies
import GameTree
    ( GameTreeStatus,
      HistoryTrace,
      PlayoutArgument,
      KillerMoves,
      PlayoutEvaluator(..),
      GameTree(GLeaf),
      BoardIndex,
      PlayerIndex,
      getPlayerIdx,
      getPlayerColour,
      getBoardIdx,
      getBoard,
      getCurrentInternalBoard,
      getPlayerNum,
      getHistoryTrace,
      getPlayoutArgument,
      updatePlayerIdx,
      updateBoardIdx,
      setBoard,
      modifyCurrentInternalBoard,
      getRandGen,
      setRandGen,
      getBoardIndex,
      getChildren,
      getVisits,
      getTransform,
      repaintBoard,
      editNodeValue,
      editNodeChildren,
      estimateNode, setCurrentInternalBoard, currentPlayerMovesList, getInternalBoards )
import Board
    ( Transform,
      Board,
      Colour,
      playerColourList,
      replace,
      projectMove, Pos )
import System.Random ( Random(randomR), RandomGen(split), StdGen )
import Zobrist ( hash, winStateDetect )
import Data.List ( elemIndex, elemIndices )
import Control.Monad.State ( runState, MonadState(get), State)
import Control.Parallel ( par, pseq )
import RBTree ( rbInsert, rbSearch )
import Configuration ( boardEvaluation, isMidgame, isEndgame, isOpening, ifExistMidgame )
import Minimax
    ( TreeType(..), winStateDetermine, moveEvaluation, mEvaluation, moveOrder )
import BFS (centroid)


-- during the selection a list of node that is chosen along with the selection, 
-- therefore, a list of chosen nodes (the board index as well as the internal board hash) should be maintained for later backpropagation
-- the selected node list is first-in-first-out style
push :: a -> [a] -> [a]
push x xs = xs ++ [x]
pop :: [a] -> (a, [a])
pop [] = error "Empty container"
pop [x] = (x, [])
pop (x:xs) = (x, xs)

-- given a change of position, create a leaf node 
makeLeaf :: Transform -> State GameTreeStatus GameTree
makeLeaf transform = do bi <- getBoardIdx
                        pn <- getPlayerNum
                        -- add an increment of the board index, such that it won't be duplicate
                        bi `par` pn `pseq` updateBoardIdx
                        -- allocate the board index and the win list with the same length of total players
                        return (GLeaf bi transform (replicate pn 0))

-- generate a random index with a given range
randomIndex :: Int -> State GameTreeStatus Int
randomIndex len = do gen <- getRandGen
                     let (randomValue, newGen) = randomR (0, len-1) gen
                     -- reset the random number generator after a random index is made
                     setRandGen (snd (split newGen))
                     return randomValue

-- generate a random value from 0 to 100, for random percentage decision making
randomPercentage :: Int -> State GameTreeStatus Bool
randomPercentage n = do gen <- getRandGen
                        let (randomValue, newGen) = randomR (0, 100) gen
                        setRandGen (snd (split newGen))
                        -- return the possibility of certain percentage
                        return $ randomValue <= n

-- return the maximum entity's index of a list, if there exist multiple maximum entities then select one of them randomly
randomMaxSelection :: Ord a => [a] -> State GameTreeStatus Int
randomMaxSelection []  = error "Selection: no node for selecting"
randomMaxSelection xs  = let is = maximum xs `elemIndices` xs
                         in  case is of
                                [idx] -> return idx
                                _ -> do ri <- randomIndex (length is)  -- random index of the maximum values' indices
                                        return $ is !! ri -- return the maximum value's index

--Phase Operators--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- normal MCTS is divided into four phases: selection, expansion, playout and backpropagation

-- the first phase is to select one resulting boards with the maximum profit/score which is calculated based on different formulas
-- select the nodes from the root of the game tree to a leaf and produce lists of traversed nodes (indices) and the corresponding board hashes
-- need to be noticed that a terminal node (winning node) is also a leaf, therefore, stop the selection when meeting a leaf could also end the search earlier
selection :: GameTree -> [Int] -> [Int] -> State GameTreeStatus ([Int], [Int], GameTree)
selection node indexList hashList = let children = getChildren node
                                    in  -- return the node and trace for the next stage when meeting a leaf 
                                        -- a leaf might be either a terminal point or just a node that hasn't been expanded
                                        if null children then return (indexList, hashList, node)
                                        else do -- produce a list of profits for all children
                                                scores <- mapM (estimateNode (getVisits node)) children
                                                -- return the maximum entity's index
                                                maxIdx <- randomMaxSelection scores
                                                let -- select the child and the corresponding movement with the maximum score 
                                                    selectedNode = children !! maxIdx
                                                    movement = getTransform selectedNode
                                                -- update the internal board for the current player
                                                nodeInternalState <- modifyCurrentInternalBoard movement
                                                setCurrentInternalBoard nodeInternalState
                                                -- record the selected node for guiding the later backpropagation 
                                                let newindexList = push (getBoardIndex selectedNode) indexList
                                                    newhashList = push (hash nodeInternalState) hashList
                                                -- update the external board
                                                newboard <- repaintBoard movement
                                                setBoard newboard
                                                -- update the game turn
                                                updatePlayerIdx
                                                -- start the selection at the next layer with the chosen node and updated records
                                                newindexList `par` newhashList `pseq` selection selectedNode newindexList newhashList

-- here, the process expands a node based on the possible moves that could be made by the current player, and assign each resulting board with new index and make them leaves
-- the expansion could apply various policies such as restricting the generated new nodes, or avoid the backward moves to be produced
-- in this function, it accepts a tree node and returns the same node but with children
expansion :: GameTree -> State GameTreeStatus GameTree
expansion node = let children = getChildren node
                 in  if -- the node passed from the selection should only be leaf, otherwise, it an error
                        not (null children) then error "Can only expand not expanded node"
                  else do ms <- currentPlayerMovesList
                          colour <- getPlayerColour
                          ps <- getCurrentInternalBoard
                          -- generate the leaves for movements that are accepted for expanding
                          newChildren <- ms `par` colour `par` ps `pseq` mapM makeLeaf (expandPolicy colour ms ps)
                          -- the new generated nodes will become the children of the input node 
                          return (editNodeChildren newChildren node)

-- the policy of how a board could lead to different resulting boards
expandPolicy :: Colour -> [Transform] -> [Pos] -> [Transform]
expandPolicy colour xs ps
    | not $ null advance = advance  -- only consider advance moves if there are enough
    | otherwise = xs                -- otherwise, all moves are accepted
    where
        -- the available movements are divided into two categories: advance and non-advance
        -- the one that provides an increment in distance is an advance, otherwise, non-advance
        advance = filter ((> 0) . distanceChange) xs
        -- project the movement from external board to the occupied board and return the change of distance
        distanceChange move = moveEvaluation (projectMove colour move)

-- after the playout, a win is known from the game simulation, and should be update back to the traversed nodes in the selection phase
-- here, it will updates the node's stored wins, as well as appending children to the the chosen leaf
-- the process containing traversing child nodes from the root until reaching the last node in the trace
backpropagation :: PlayerIndex -> [Int] -> GameTree -> GameTree -> State GameTreeStatus GameTree
backpropagation _ [] node _ = return node
backpropagation winIdx itrace node expandedNode =
                                             let (bi, rtrace) = pop itrace
                                                 -- first check if the current node is the one being expanded in previous phases
                                                 tempNode = (if getBoardIndex node == getBoardIndex expandedNode then expandedNode else node)
                                                 -- this is for retrieving the children from the new expanded node and update them
                                                 children = getChildren tempNode
                                             in  -- find the node that is chosen during the selection
                                                 case elemIndex bi (map getBoardIndex children) of
                                                    Nothing -> error "Trace incorrect"
                                                    Just idx -> do let selectedNode = children !! idx
                                                                       -- update the win list by incrementing a player's wins by 1
                                                                       childWithNewWins = editNodeValue winIdx selectedNode
                                                                   -- recursively digging down on the that child until the trace is empty
                                                                   childWithNewChildren <- backpropagation winIdx rtrace childWithNewWins expandedNode
                                                                   -- finally update the new child of the current node
                                                                   let newChildren = replace idx childWithNewChildren children
                                                                   return (editNodeChildren newChildren node)

-- the update of the history trace
editHT :: [Int] -> PlayerIndex -> HistoryTrace -> State GameTreeStatus HistoryTrace
editHT [] _ ht = return ht
editHT (h:hs) winIdx ht = do pn <- getPlayerNum
                             let nht = (case rbSearch h ht of
                                            -- if not then add an initial win list
                                            Nothing -> rbInsert h (initWins pn) ht
                                            -- otherwise, update the wins of the certain player
                                            Just ws -> rbInsert h (replace winIdx ((ws!!winIdx)+1) ws) ht)
                             -- keep processing until is trace is empty
                             editHT hs winIdx nht
    where
        initWins pn = replace winIdx 1 (replicate pn 0)

--Simulation-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- how the game simulation is done during the playout phase

-- different approaches for estimating the current board state during the playout stage
-- 0. random choice policy
-- 1. move distance evaluation
-- 2. mixed strategy of lookup table and centroid heuristic
-- 3. midgame-only percentage-based minimax search equipped with centroid heuristic

-- random greedy policy with certain percentage of choosing the optimal option while the maintaining certain randomness factor for avoiding cycling
switchEvaluator :: PlayoutArgument -> [Transform] -> State GameTreeStatus Transform
switchEvaluator (evaluator, depth, percentage) tfs = do -- first check whether random choice will be taken place here (5% of chance)
                                                         optimalChoice <- randomPercentage 95
                                                         -- besides, the possibility of taking the minimax search during the midgame is also generated here
                                                         isMinimaxApplied <- randomPercentage percentage
                                                         -- or just the policy is set as random choice
                                                         if optimalChoice `seq` not optimalChoice
                                                         then do -- purely random choice
                                                                 idx <- randomIndex (length tfs)
                                                                 return (tfs !! idx)
                                                         else -- otherwise, each evaluator will lead to different estimation
                                                              isMinimaxApplied `seq`
                                                              case evaluator of
                                                                -- normal random choice (selective randomness)
                                                                Random -> do randomAdvance tfs
                                                                -- pure distance-based heuristic
                                                                Move -> moveEvaluator tfs
                                                                -- lookup table mixed with global board state heuristic
                                                                Board -> boardEvaluator tfs
                                                                -- the percentage-based minimax search 
                                                                -- in this case, the minimax search is no longer the main evaluation but enhancement could be triggered by certain possibility
                                                                -- the midgame-only variant of the percentage-based minimax search is implemented here
                                                                MParanoid -> mixedSearch isMinimaxApplied Paranoid depth tfs
                                                                MBRS -> mixedSearch isMinimaxApplied BRS depth tfs
    where
        -- to avoid wasting simulations, only the certain moves are allowed to be chosen randomly
        randomAdvance :: [Transform] -> State GameTreeStatus Transform
        randomAdvance tfs = do colour <- getPlayerColour
                               iboard <- getCurrentInternalBoard
                               let remainingMoves = expandPolicy colour tfs iboard
                               idx <- randomIndex (length remainingMoves)
                               return (remainingMoves !! idx)

        -- choose the move that could give the largest distance increment
        moveEvaluator :: [Transform] -> State GameTreeStatus Transform
        moveEvaluator tfs = do colour <- getPlayerColour
                               -- get the distance change from the projected occupied board
                               let ptfs = map (projectMove colour) tfs
                                   scores = map moveEvaluation ptfs
                               idx <- randomMaxSelection scores
                               return (tfs !! idx)

        -- choose the move that return the best score from the dataset/heuristic
        boardEvaluator :: [Transform] -> State GameTreeStatus Transform
        boardEvaluator tfs = do -- generate a list of new internal boards
                                psList <- mapM modifyCurrentInternalBoard tfs
                                alternativeMove <- moveEvaluator tfs
                                -- let scores = map centroid psList
                                if psList `par` alternativeMove `pseq` ifExistMidgame psList 
                                then do {-idx <- randomMaxSelection scores
                                          return (tfs !! idx)-}
                                        return alternativeMove
                                else do let scores = map boardEvaluation psList
                                        idx <- randomMaxSelection scores
                                        return (tfs !! idx)

        -- apply the depth-limited minimax-based search and return the provided optimal move
        minimaxSearch :: Int -> TreeType -> State GameTreeStatus Transform
        minimaxSearch 0 _ = error "the depth should be larger than zero"
        minimaxSearch depth treetype = do (_, ri, _, eboard, iboards, pn, _, _, _) <- get
                                          return $ fst $ mEvaluation depth (ri, eboard, iboards, pn, (-999, 999), treetype) [ri]

        -- since applying the minimax search throughout the whole simulation is too costly, it will only be applied in certain condition
        mixedSearch :: Bool -> TreeType -> Int -> [Transform] -> State GameTreeStatus Transform
        mixedSearch flag treetype depth tfs = do iboard <- getCurrentInternalBoard
                                                 if isMidgame iboard && flag then minimaxSearch depth treetype
                                                 else boardEvaluator tfs

-- game simulation from a certain board state till the end of the game, and every move made in the simulation is generated based on certain policy
playout :: Int -> State GameTreeStatus PlayerIndex
playout moveCounts =
                do pn <- getPlayerNum
                   let turns = getTurns moveCounts pn
                   if turns >= 1000 then do -- stop the playouts if costing too much time
                                            iboards <- getInternalBoards
                                            -- if exceeds the set threshold, stops the simulation and return the player with the most centroid value as the winner
                                            let scores = map centroid iboards
                                            randomMaxSelection scores
                   -- otherwise, process normally                                          
                   else do -- first get all of the available movements
                           tfs <- currentPlayerMovesList
                           pi <- getPlayerIdx
                           pa <- getPlayoutArgument
                           colour <- getPlayerColour
                           -- choose one of the expanded boards as the performed movement based on certain strategy
                           resultedMove <- pa `par` tfs `pseq` switchEvaluator pa tfs
                           -- update internal board state
                           newps <- modifyCurrentInternalBoard resultedMove
                           setCurrentInternalBoard newps
                           -- reflect the change to the external board
                           nboard <- repaintBoard resultedMove
                           -- check the soft win state of current player after the movement is made, if wins then returns, else keeps iterating
                           if colour `par` nboard `pseq` winStateDetermine colour nboard then return pi
                           else do -- next player's turn
                                   updatePlayerIdx
                                   -- update the new board state
                                   setBoard nboard
                                   -- record the moves being made so far
                                   playout (moveCounts + 1)

-- get the game turns based on the total movements made so far as well as the total players
getTurns :: Int -> Int -> Int
getTurns moves pn = ceiling (fromIntegral moves / fromIntegral pn)

--MCTS Body--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- the MCTS structure that first selects the node with largest profits, then expands it, 
-- and play simulations on the selected node, and finally feedback the search tree

-- the function connects the four phases as well as inserting win state check between each phase in order to end the search earlier and discover potential error
mcts :: GameTree -> State GameTreeStatus (StdGen, GameTree, BoardIndex, HistoryTrace)
mcts tree = do -- first start the selection with empty container, and will then receive the container full of entities
               (idxList, hashList, lastnode) <- selection tree [] []
               -- then determine if the received board is already a won board
               -- this means that some players are taking suicidal actions which could cause other players to win even it's not their turns
               -- this is not possible for normal win state determination
               -- but here, in order to prevent players leaving a piece in their home bases blocking other players, the win condition is looser
               -- therefore, the check should consider all players
               iboards1 <- getInternalBoards
               let winIdx1 = checkPlayersWinState iboards1
               -- the reason why this check is necessary it that as the tree gradually grows, it will eventually reach the goal state, therefore, 
               -- need to distinguish them in advance, otherwise, playout could be misled 

               -- directly jump to the backpropagation phase if win state is detected
               if winIdx1 /= -1 then do newTree <- backpropagation winIdx1 idxList tree lastnode
                                        ht <- getHistoryTrace
                                        newHistory <- editHT hashList winIdx1 ht
                                        bi <- getBoardIdx
                                        -- the random number generator is necessary to be returned for the next iteration of four phases
                                        gen <- getRandGen
                                        -- get the new tree and game history
                                        return $ newTree `par` newHistory `pseq` (gen, newTree, bi, newHistory)

               -- otherwise, the expansion is computed, the last node chosen is expanded with new children
               else do expandednode <- expansion lastnode
                       -- additional selection is done on the new children nodes, and the selected node will then be passed to the playout phase
                       (idxList2, hashList2, _) <- selection expandednode idxList hashList
                       -- but before that, the check is necessary to be done in advance as well 
                       iboards2 <- getInternalBoards
                       let winIdx2 = checkPlayersWinState iboards2
                       if winIdx2 /= -1 then do -- here, the playout phase will be skipped, and the backpropagation will take place earlier
                                                newTree2 <- backpropagation winIdx2 idxList2 tree expandednode
                                                bi <- getBoardIdx
                                                ht <- getHistoryTrace
                                                newHistory2 <- editHT hashList2 winIdx2 ht
                                                gen <- getRandGen
                                                return $ newTree2 `par` newHistory2 `pseq` (gen, newTree2, bi, newHistory2)

                       else -- then, the playout could be started 
                            do winIdx3 <- playout 0
                               -- after the playout is completed, apply the backpropagation for renewing the search tree
                               newTree3 <- backpropagation winIdx3 idxList2 tree expandednode
                               bi <- getBoardIdx
                               ht <- getHistoryTrace
                               newHistory3 <- editHT hashList2 winIdx3 ht
                               gen <- getRandGen
                               return $ newTree3 `par` newHistory3 `pseq` (gen, newTree3, bi, newHistory3)



-- check the win state of all players, since the suicidal action is allowed here and it can be performed either randomly or on purpose
-- any value other than -1 means a win is reached
-- different from the soft win of the playout phase, here, the win is determined in the standard way, 
-- this is because if only depend on the looser win determination, there could occur error when the softer win state is reached but not the actual win state
-- in other words, a root without children will be returned because this root is defined as a win, therefore, no move should be made, which is not applicable in the experimental environment
checkPlayersWinState :: [[Pos]] -> Int
checkPlayersWinState iboards = let ws = map winStateDetect iboards
                               in  case elemIndices True ws of
                                    [] -> -1
                                    [x] -> x
                                    -- although suicidal movement is allowed because it won't affect the mechanism of MCTS, 
                                    -- it is not allowed to more than one players win at the same time because the game is zero-sum
                                    _ -> error "Multiple players win at the same time"