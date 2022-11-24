module MCTS where

import GameTree
import RBTree
-- Normal MCTS is divided into four phases: selection, expansion, playout and backpropagation
-- the first phase is to select one resulting board with the maximum profit

-- the whole structure requires a state monad that stores the constructed game tree
-- the score is calculated based on different strategies: UCT for MCTS selection or move-based evaluation of minimax algorithm



-- the structure of MCTS, containing state monad: the game tree itself, 
-- the history heuristic used for progressive history and the route/path for backpropagation
-- selection :: GameTree -> SB (GameTree, HistoryTree) Trace
-- selection (GRoot _ ts) = do (gt, ht) <- stState
--                             let scoreList = map (estimateNode ht 0) ts
--                                 idx = maxIndex scoreList -- choose the one with the largest benefit
--                                 nextNode = ts !! idx
                            


-- might have different definitions of profits
-- selectNode :: GameTree -> GameTree
-- selectNode t@(GLeaf _ b) = let newNode = expandNode t
--                            in  selectNode newNode
-- selectNode (GNode _ _ ts) = let maxScore = maximum (map getScore ts)
--                                 idx = head (elemIndices maxScore (map getScore ts))
--                             in  ts !! idx

-- -- might have restriction of expanded nodes
-- expandNode :: GameTree -> GameTree
-- expandNode (GLeaf score board) = let boardCentroid = centroid board
--                                      moveList = dListForBoard board
--                                      boardList = flipLists board boardCentroid moveList -- [(O, C)]
--                                      frontwardList = filter ((>= boardCentroid) . snd) boardList -- remove the non-frontward moves
--                                  in  GNode score board (map (makeNode . fst) frontwardList)
-- expandNode (GNode score board t) = GNode score board t

-- random simulation, but could add some strategies
-- playoutNode :: GameTree -> Int
-- playoutNode (GNode s _ ts) = s
-- playoutNode (GLeaf )

-- the backup needs a trace

-- retrieve a score of an internal node
estimateNode :: HistoryTree -> Visits -> GameTree -> Double
estimateNode _ _ (GRoot _ _) = 0
estimateNode _ _ (GLeaf _ _) = 0
estimateNode ht pv (GNode (_, h) _ w v _) = estimate ht h w v pv

-- the selection strategy for "profit" of a node
estimate :: HistoryTree -> Hash -> Wins -> Visits -> Visits -> Double
estimate t h w v pv = calculateUCT w v pv + calculatePH t h w v

-- UCT formula
calculateUCT :: Wins -> Visits -> Visits -> Double
calculateUCT wins visit pvisit = (fromIntegral wins / fromIntegral visit) + 0.5 * sqrt (log (fromIntegral pvisit) / fromIntegral visit)

-- Progressive History formula
calculatePH :: HistoryTree -> Hash -> Wins -> Visits -> Double
calculatePH t h w v = case rbSearch h t of
                        Nothing -> 0 -- if this node is not played before
                        Just (w1, v1) -> (fromIntegral w1 / fromIntegral v1) * (5 / fromIntegral (v - w + 1))