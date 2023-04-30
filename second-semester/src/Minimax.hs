module Minimax where
-- the module of the minimax search: Paranoid and BRS, with limited search depth and allow multiple players
-- the section aims to provide a helpful movement decision method during the playout phase

import Board
    ( appendColour,
      colourIndex,
      defaultMove,
      destinationList,
      getColour,
      getElement,
      goalBase,
      isOccupied,
      projectMove,
      removeByIdx,
      repaintPath,
      replace,
      reversion,
      Board,
      BoardPos,
      Colour,
      Pos,
      Transform )
import Control.Monad.State
    ( State, evalState, MonadState(put, get), runState )
import GameTree
    ( pairArrange, playerColour, turnBase, KillerMoves, PlayerIndex )
import Control.Parallel ( par, pseq )
import Zobrist ( flipBoard )
import Data.List ( elemIndex, nub, sortBy )
import Control.Monad.Extra ( concatMapM )
import BFS (centroid)

-- the search tree can be categorized into two groups, the paranoid and BRS forms
-- the two forms of search tree arrange the Max and Min nodes differently
-- Paranoid tree policy treats the root layer's nodes as Max while any other nodes are Min
-- BRS does the similar way but instead of following the regular turn base, it combines all the opponents' node in one layer 
data TreeType = Paranoid | BRS deriving (Eq, Show, Read)

-- the new tree status applied during computing the Minimax search
type MGameTreeStatus = (-- the root layer player's index, should be static
                        PlayerIndex,
                        -- the current board state 
                        Board,
                         -- the list of positions of the internal board for each player
                        [[Pos]],
                        -- the total players, should be static
                        Int,
                        -- the alpha-beta pair, used for pruning the branches of the built search tree
                        AlphaBeta,
                        -- the choice of shallow minimax search, should be static
                        TreeType
                        )

-- the alpha and beta value, alpha value represents the solution of Max node, and beta for Min node
-- this pair is not consistent throughout the search tree, it might be switched, for instance, 
-- the maximum value of a subtree could be the minimum value of its parent
type AlphaBeta = (Double, Double)

-- the win state detection here is not just checking the internal state, it considers not only the normal winning condition, 
-- but also the potential blocking, therefore, a board state is defined as winning for a player if the corresponding goal base is filled and at least one of the pieces belongs to him/her
winStateDetermine :: Colour -> Board -> Bool
winStateDetermine c b = let -- get the goal base positions on the external board of certain player 
                            goalPos = map (reversion c) goalBase
                            -- and get the corresponding occupied state
                            goalBoardPos = evalState (do mapM getElement goalPos) b
                            -- check if the two conditions are satisfied
                            flag1 = isFull goalBoardPos
                            flag2 = existColour c goalBoardPos
                        in  flag1 `par` flag2 `pseq` (flag1 && flag2)
    where
        isFull :: [BoardPos] -> Bool
        isFull = foldr ((&&) . isOccupied) True
        
        existColour :: Colour -> [BoardPos] -> Bool
        existColour c bs = Just c `elem` map Board.getColour bs

--Enhancements------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
-- addition to the standard work flow, some domain-independent techniques were added to fasten the search

-- return the distance between two positions
dist :: Pos -> Pos -> Double
dist (x1, y1) (x2, y2) = sqrt (fromIntegral (x1 - x2)^2 + fromIntegral (y1 - y2)^2)

-- a simple move evaluator of how close the changed piece is to the goal state, measuring the forward distance to the goal
-- this evaluation is more straightforward as it only considers the distance change
moveEvaluation :: (Pos, Pos) -> Double
moveEvaluation (p1, p2) = let dist1 = dist p1 (0, 6)
                              dist2 = dist p2 (0, 6)
                           in dist1 `par` dist2 `pseq` (dist1 - dist2) -- the larger the better

-- accept a list of moves and a distance-based heuristic, rank them from high to low, 
-- the one gives the largest distance increment will be placed at the front
moveOrder :: [Transform] -> [Transform]
moveOrder ms = let -- calculate the distance change for each move
                   result = assignDistance ms
                   -- rank them based on the distance change
                   sorted = sortBy (\(a, _) (b, _) -> compare b a) result
               in  map snd sorted -- omit the distance values
    where
        -- zip with the assoicated distance
        assignDistance :: [Transform] -> [(Double, Transform)]
        assignDistance ms = map getDistance ms
        
        -- get the distance change of the internal position          
        getDistance :: Transform -> (Double, Transform)
        getDistance m@(from, to) = case getColour from of
                                    Nothing -> error "Invalid generated movement"
                                    Just co -> let internalPos = projectMove co m
                                                   dis = moveEvaluation internalPos
                                               in  (dis, m)

-- based on the colour of the movement, assign the corresponding player's index
assignIndex :: [Transform] -> Int -> [PlayerIndex]
assignIndex ms pn = map (getPlayerIndex pn) ms
    where
        getPlayerIndex :: Int -> Transform -> PlayerIndex
        getPlayerIndex pn (from, _) = case getColour from of
                                        Nothing -> error "invalid movement"
                                        Just colour -> case colourIndex colour pn of
                                                            Nothing -> error "invalid colour"
                                                            Just index -> index

-- an enhancement that instead of investigating all nodes of a layer, only a certain number of nodes are allowed to be considered
-- this saves a lot of time but could miss some decisive moves, therefore, should be applied together with the more ordering
kbestpruning :: [Transform] -> [Transform]
kbestpruning tfs = take 5 $ moveOrder tfs -- 5 is a fixed value, and can be extended when the efficiency is significantly improved

-- a combination of k-best pruning and killer moves
-- given a list of available movements, first take the existing killer moves away and reorder the movements based on distance increment
-- after that, append the found killer moves to the front of the list
reorderMovements :: [Transform] -> Int -> ([PlayerIndex], [Transform])
reorderMovements ms pn = let orderedMoves = kbestpruning ms
                             indices = assignIndex orderedMoves pn
                         in  (indices, orderedMoves)

--Minimax tree search------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 

-- when it comes to BRS, the search tree becomes different where the second layer's node is no longer one opponent, but all opponents
otherPlayers :: Int -> PlayerIndex -> [PlayerIndex]
otherPlayers pn ri = filter (/=ri) [0 .. pn - 1]

-- therefore, it needs a different turn switch mechanism than the standard turn base
turnBaseBRS :: Int -> PlayerIndex -> PlayerIndex -> [PlayerIndex]
turnBaseBRS pn ri pi = if ri /= pi then [ri] else otherPlayers pn ri

-- for a certain player, return a list of available movements on the current board
mplayerMovesList :: MGameTreeStatus -> PlayerIndex -> [Transform]
mplayerMovesList (_, eboard, iboard, pn, _, _) pi = let -- the colour of the player's piece
                                                        colour = playerColour pi pn 
                                                        -- the internal positions of a player
                                                        ps = (iboard !! pi) 
                                                        -- revert those position to the external board
                                                        bs = colour `par` ps `pseq` map (appendColour colour . reversion colour) ps 
                                                        -- generate available movements based on the external board state
                                                        ds = evalState (do mapM destinationList bs) eboard 
                                                    in  -- zip the start and the end, ensuring that they are listed in pairs
                                                        pairArrange bs ds 

-- the pruning is available based on evaluating the nodes below:
-- normally, when a Min node's (beta) value is less than or equal to (<=) any parent (Max)'s alpha value, that branch can be pruned
-- on the other hand, when a Max node's (alpha) value is larger than or equal to (>=) any parent (Min)'s beta value, the pruning can be also applied as well
-- in the multi-player version Minimax like Paranoid, the switch of Max and Min nodes are not done layer by layer, 
-- for instance, a Paranoid tree is having one Max layer and many of Min layers, 
-- but the regular pruning could be still applicable, the return is consistent that that Max node always returns alpha value while Min returns beta

--                  root          Given board state
--                 /    \
--                A      B        The new board states made by the root player
--               / \    / \
--              C   D  E   F      The new board states made by the first encountering player (one of the opponents, Paranoid), or all opponents (BRS)
--             /     \/     \
--            G     ....     H    The follow up layers (the next opponent, Paranoid) (or the root layer, BRS)


-- given a depth and tree status, and the player index to start with, 
-- generates the potential movements based on current board and send part of them to the next layer, either Max or Min, until reaching the bottom
-- where the board is eventually evaluated based on certain heuristic, after that, return the optimal move with its profit
mEvaluation :: Int -> MGameTreeStatus -> [PlayerIndex] -> (Transform, Double)
-- pass to the evaluation when reach the bottom of the fixed-depth tree
mEvaluation 0 st _ = (defaultMove, nEvaluation st)
mEvaluation height st@(ri, _, ps, pn, _, _) is = let (indices, rmoves) = reorderMovements (concatMap (mplayerMovesList st) is) pn
                                                     -- determine the layer to put the resulting movements
                                                 in  if is == [ri] then maxEvaluation (height - 1) st rmoves defaultMove indices
                                                     else minEvaluation (height - 1) st rmoves defaultMove indices

-- the difference between two minimax search: the Paranoid search follows the regular order base, 
-- while the BRS considers all players other than the root as a layer, so it actually evaluates a list of boards from different players 
treeSearch :: Int -> MGameTreeStatus -> PlayerIndex -> Double
treeSearch 0 st pi = snd $ mEvaluation 0 st [pi]
treeSearch h st@(ri, _, _, pn, _, Paranoid) pi = snd $ mEvaluation h st [turnBase pn pi]
treeSearch h st@(ri, _, _, pn, _, BRS) pi = snd $ mEvaluation h st (turnBaseBRS pn ri pi)

-- for handling the Max nodes 
-- here first sync the game status based on the given moves, and then passes it to the next layer until reaching the bottom (pre-set depth)  
-- while maintaining the best score and the corresponding movement                                                       
maxEvaluation :: Int -> MGameTreeStatus -> [Transform] -> Transform -> [PlayerIndex] -> (Transform, Double)
-- Max layer should return alpha value
maxEvaluation _ (_, _, _, _, (alpha, _), _) [] bestMove _ = (bestMove, alpha) 
maxEvaluation _ (_, _, _, _, (alpha, _), _) _ bestMove [] = (bestMove, alpha)
maxEvaluation height st@(ri, eboard, iboards, pn, ab@(alpha, beta), tt) (m:ms) bestMove (pi:pis) =
                                                        -- first update the status for the next layer
                                                        let currentColour = playerColour pi pn
                                                            newiboard = flipBoard (iboards !! pi) (projectMove currentColour m)
                                                            newiboards = replace pi newiboard iboards
                                                            neweboard = repaintPath eboard m
                                                            -- generate the status to be delivered to the next layer
                                                            nextTurnState = neweboard `par` newiboards `pseq` (ri, neweboard, newiboards, pn, ab, tt)
                                                            -- pass the status to the next layer and retrieve the best score from that layer
                                                            score = treeSearch height nextTurnState pi
                                                            -- compare the score from layers below with the existing alpha value
                                                            -- decide if needed to update the best move and score
                                                            (newBestMove, newAlpha) = if score >= alpha then (m, score) else (bestMove, alpha)
                                                            -- prune the branch if found a proof that the parent won't choose this branch
                                                        in  if newAlpha >= beta then (bestMove, beta)
                                                            else -- compute the next movement in the list at the same layer
                                                                 maxEvaluation height (ri, eboard, iboards, pn, (newAlpha, beta), tt) ms newBestMove pis
                                                                
-- similar to above but different in comparing the returned score
minEvaluation :: Int -> MGameTreeStatus -> [Transform] -> Transform -> [PlayerIndex] -> (Transform, Double)
-- Min layer returns beta value
minEvaluation _ (_, _, _, _, (_, beta), _) [] bestMove _ = (bestMove, beta) 
minEvaluation _ (_, _, _, _, (_, beta), _) _ bestMove [] = (bestMove, beta)
minEvaluation height st@(ri, eboard, iboards, pn, ab@(alpha, beta), tt) (m:ms) bestMove (pi:pis) =
                                                        let currentColour = playerColour pi pn
                                                            newiboard = flipBoard (iboards !! pi) (projectMove currentColour m)
                                                            newiboards = replace pi newiboard iboards
                                                            neweboard = repaintPath eboard m
                                                            nextTurnState = neweboard `par` newiboards `pseq` (ri, neweboard, newiboards, pn, ab, tt)
                                                            score = treeSearch height nextTurnState pi
                                                            (newBestMove, newBeta) = if score <= beta then (m, score) else (bestMove, beta)
                                                        in  if newBeta <= alpha then (bestMove, alpha)
                                                            else minEvaluation height (ri, eboard, iboards, pn, (alpha, newBeta), tt) ms newBestMove pis

-- evaluate the bottom node of the search tree
-- since the evaluation should be made based on the root's perspective, the evaluation of the other layer player is done by evaluating how the root player could benefit
nEvaluation :: MGameTreeStatus -> Double 
nEvaluation st@(ri, eboard, iboard, pn, _, _)
    -- only care about the root layer's score, if root player wins, returns the maximum gain
    | winStateDetermine rolour eboard = 28 
    -- if no player is winning, how a board is evaluated is based on how the root player could play on this board
    | otherwise = let pms = map (projectMove rolour) (mplayerMovesList st ri) 
                      scores = map moveEvaluation pms
                  in  if null scores then error (show eboard) else maximum scores
    where
        rolour = playerColour ri pn