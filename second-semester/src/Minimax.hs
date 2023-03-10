module Minimax where
-- the module of the shallow minimax search: Paranoid and BRS
import Board
import Control.Monad.State
import GameTree
import Configuration
import Control.Parallel
import Zobrist
import Data.Time
import Data.List
import System.Environment



-- the node is divided into two groups, Max for root layer and the rest is Min 
-- the search tree is also divided into two groups, the paranoid and BRS forms
data TreeType = Paranoid | BRS deriving (Eq, Show, Read)
-- the new tree status with less information to consider
type MGameTreeStatus = (PlayerIndex, -- the root layer player's index, should be static
                        Board, -- the board state delivered from the parent
                        [[Pos]], -- the list of positions of the internal board for each player
                        Int, -- the total players, should be static
                        AlphaBeta,
                        TreeType, -- the choice of shallow minimax search, should be static
                        Int -- the parameter of k-best pruning
                        -- KillerMoves -- a list of killer moves, in hashed form
                        )
type AlphaBeta = (Int, Int) -- the alpha and beta value, alhpa value represents the solution of Max node, and beta for Min node
type KillerMove = Maybe (Pos, Pos)
type KillerMoves = [(KillerMove, KillerMove)] -- the two last moves that were best or caused a cutoff

-- the alpha and beta pair is not consistent throughout the search tree, it might be switched, for instance, 
-- the maximum value of the subtree could be the minimum value of its parent, when the layers of Max and Min are switched

-- ghc -main-is Minimax Minimax.hs -O2 -outputdir dist
{-
main = do arg <- getArgs
          start <- lookupTable `pseq` getCurrentTime
          let st@(ri, board, pl, pn, ab, tt) = iStatus (read $ arg !! 1)
          print $ mEvaluation (read $ head arg) st ri
          end <- getCurrentTime
          print $ diffUTCTime end start
-}

iStatus :: TreeType -> MGameTreeStatus
iStatus tt = (0, eboard, internalBoards, pn, (-999, 999), tt, 10)
    where
        pn = 3
        eboard = eraseBoard (playerColourList pn)
        internalBoards = map (convertToInternalBoard eboard) (playerColourList pn)
        -- initialKillerMoves = replicate depth (Nothing, Nothing)


-- the pruning is available based on evaluating the nodes below
-- normally, when a Min node's (beta) value is larger than any parent (Max)'s alpha value, that branch can be pruned
-- on the other hand, when a Max node's (alpha) value is less than any parent (Min)'s beta value, the pruning can be applied as well
-- however, in a multi-player version, the switch of Max and Min nodes are not done layer by layer, 
-- for instance, a paranoid tree is having one Max layer and many of Min layers but the regular pruning could be applicable for Paranoid and BRS forms
-- besides, the return process is consistent and the Max node always returns alpha value while Min returns beta

--                  root          Given board state
--                 /    \
--                A      B        The new board states made by the root player
--               / \    / \
--              C   D  E   F      The new board states made by the first encountering player (one of the opponents), or all opponents
--             /     \/     \
--            G     ....     H    The follow up layers

-- given a depth and tree status, and the current player to start with, first generats its movements based on current board
-- and send it to evaluate the resulting boards from the movements
-- besides of returning the optimal score it could retrieve, it will also return the corresponding movement that generates that score
mEvaluation :: Int -> MGameTreeStatus -> PlayerIndex -> (Transform, Int)
mEvaluation 0 st pi = ((U(-1,-1), U(-1,-1)), nEvaluation st pi)
mEvaluation depth st@(ri, board, pl, pn, (alpha, beta), _, _) pi = let ms = mplayerMovesList st pi -- provide a list of possible movements
                                                                   in  -- evaluate a list of movements, just like a list of nodes
                                                                       if ri /= pi then minEvaluation depth st pi ms (U(-1,-1), U(-1,-1))
                                                                       else maxEvaluation depth st pi ms (U(-1,-1), U(-1,-1))

-- the evaluation here will first sync the game status based on the given movements, and then passes it to the next layer until reaching the bottom (set depth)  
-- while maintaining the best score and the corresponding movement                                                               
maxEvaluation :: Int -> MGameTreeStatus -> PlayerIndex -> [Transform] -> Transform -> (Transform, Int)
maxEvaluation _ (_, _, _, _, (alpha, _), _, _) _ [] tf = (tf, alpha) -- Max layer returns alpha value
maxEvaluation depth st@(ri, board, pl, pn, ab@(alpha, beta), tt, k) pi (m:ms) tf = let nib = flipBoard (pl !! pi) (projectMove (playerColour pi pn) m) -- edit the internal positions
                                                                                       npl = replace pi nib pl
                                                                                       neb = repaintPath board m -- edit the new board state
                                                                                       newDepth = depth - 1
                                                                                       nst = (ri, neb, npl, pn, ab, tt, k) -- retrieve new tree status
                                                                                       score = maximum $ treeSearch newDepth nst pi -- decide in which way keep (Paranoid or BRS style) digging down
                                                                                       -- and calculate the maximum score
                                                                                       (newtf, newAlpha) = if score >= alpha then (m, score) else (tf, alpha) -- update the alpha value and the best move so far
                                                                                   in  if newAlpha >= beta then (tf, beta) -- prune the branch if found a proof that the paranet won't choose this branch
                                                                                       else maxEvaluation depth (ri, board, pl, pn, (newAlpha, beta), tt, k) pi ms newtf -- get to the next movements provided in a list
-- similar to above but different in the return value
minEvaluation :: Int -> MGameTreeStatus -> PlayerIndex -> [Transform] -> Transform -> (Transform, Int)
minEvaluation _ (_, _, _, _, (_, beta), _, _) _ [] tf = (tf, beta) -- Min layer returns beta value
minEvaluation depth st@(ri, board, pl, pn, ab@(alpha, beta), tt, k) pi (m:ms) tf = let nib = flipBoard (pl !! pi) (projectMove (playerColour pi pn) m) 
                                                                                       npl = replace pi nib pl
                                                                                       neb = repaintPath board m 
                                                                                       newDepth = depth - 1
                                                                                       nst = (ri, neb, npl, pn, ab, tt, k) 
                                                                                       score = minimum $ treeSearch newDepth nst pi 
                                                                                       (newtf, newBeta) = if score <= beta then (m, score) else (tf, beta) -- update the beta value
                                                                                   in  if alpha >= newBeta then (tf, alpha)
                                                                                       else minEvaluation depth (ri, board, pl, pn, (alpha, newBeta), tt, k) pi ms newtf

-- the difference between two shallow minimax search: the Paranoid search follows the regular order base, while the BRS considers all players other than
-- the root as a layer, so it actually evaluates a list of boards from different players 
-- since it is not necessary to know about the best movement of the next layer, here it is ignored
treeSearch :: Int -> MGameTreeStatus -> PlayerIndex -> [Int]
treeSearch 0 st pi = map snd [mEvaluation 0 st pi]
treeSearch depth st@(ri, _, _, pn, _, Paranoid, _) pi = map snd [mEvaluation depth st (turnBase pn pi)]
treeSearch depth st@(ri, _, _, pn, _, BRS, _) pi = map (snd . mEvaluation depth st) (turnBaseBRS pn ri pi)
                                                     
-- when it comes to BRS, the search tree becomes different where the second layer's node is no longer one opponent, but all opponents
otherPlayers :: Int -> PlayerIndex -> [PlayerIndex]
otherPlayers pn ri = filter (/=ri) [0 .. pn - 1]
-- therefore, it needs a different turn switching mechanism
turnBaseBRS :: Int -> PlayerIndex -> PlayerIndex -> [PlayerIndex]
turnBaseBRS pn ri pi = if ri /= pi then [ri] else otherPlayers pn ri

-- evaluate the bottom node (where the depth is equal to 0) of the search tree
-- since the evaluation should be made based on the root's perspective, the evaluation of the other layer player is done by evaluating how the root player could benefit
nEvaluation :: MGameTreeStatus -> PlayerIndex -> Int
nEvaluation st@(ri, board, pl, pn, _, _, _) pi = if ri == pi then boardEvaluation (pl !! ri) -- for root, just simply measure its score
                                                 else let rolour = playerColour ri pn -- for other players, measure the possible root player's move could perform based on the changed board
                                                          rs = pl !! ri
                                                          tfs = mplayerMovesList st ri -- the avaliable root's movements
                                                          bs = map (flipBoard rs . projectMove rolour) tfs -- the resulting boards that could generated by the root player
                                                          scores = map boardEvaluation bs -- the corresponding score
                                                      in  maximum scores -- return the maximum one

-- given a certain piece's colour, return a list of avaliable movements (transforms) on the external board
mplayerMovesList :: MGameTreeStatus -> PlayerIndex -> [Transform]
mplayerMovesList (ri, board, pl, pn, ab, _, k) pi = 
                                                    let colour = playerColour pi pn -- the colour of the player's piece
                                                        ps = (pl !! pi) -- the internal positions of a player
                                                        bs = map (appendColour colour . reversion colour) ps -- project those position onto the external board
                                                        ds = evalState (do mapM destinationList bs) board -- and generate movements based on the external board state
                                                        ms = pairArrange bs ds -- zip the start and the end, ensuring that those are listed in pairs
                                                        -- before sending to the layer, k-best pruning is implemented here
                                                    in  take k (moveOrder colour ms)

-- simple move evalutor of how close the changed piece is to the goal state, 
-- an addition move evaluator is to measure the forward distance to the goal base
dist :: Pos -> Pos -> Double
dist (x1, y1) (x2, y2) = sqrt (fromIntegral (x1 - x2)^2 + fromIntegral (y1 - y2)^2)
-- return the distance of a piece to the goal state, the evaluation is more straightforward as only considering the distance
moveEvaluation :: (Pos, Pos) -> Double
moveEvaluation (p1, p2) = let dist1 = dist p1 (0, 6)
                              dist2 = dist p2 (0, 6)
                           in dist1 `par` dist2 `pseq` (dist1 - dist2) -- still the larger the better

-- accept a list of movements and an distance-based heuristic, rank them from high to low
moveOrder :: Colour -> [Transform] -> [Transform]
moveOrder co ms = 
                let pms = map (projectMove co) ms
                    dis = map moveEvaluation pms -- transfer into the internal distance
                    pairs = zip dis ms
                    sortedList = sortBy (\(a, _) (b, _) -> compare b a) pairs
                in  map snd sortedList

-- first we project the movements from external form into internal form, 
-- then we search if there exist any killer moves at the current layer of minimax search
-- if it does, then we have to take it away from the resulting list of movements, and after the movements are sorted, we can then put it to the front of the sorted list
-- normally, the killer moves are specifically existing on certain ply, therefore, each ply should store its own killer moves
-- besides, since the killer moves are specific for certain layer, if wanting to reuse those moves, it will be better to modify it for the next iteration, as the game progresses also push the layer of the search
searchKillerMoves :: Int -> KillerMoves -> [(Pos, Pos)] -> [Transform] -> ([Transform], [Transform])
searchKillerMoves depth kms ims ems = let len = length kms
                                          lay = len - depth -- 0, 1, 2
                                     in  case kms !! lay of
                                            (Nothing, Nothing) -> ([], ems)
                                            (Just km, Nothing) -> case elemIndex km ims of
                                                                    Nothing -> ([], ems)
                                                                    Just idx -> ([ems !! idx], removeItemByIdx ems idx)
                                            (Nothing, Just km) -> case elemIndex km ims of
                                                                    Nothing -> ([], ems)
                                                                    Just idx -> ([ems !! idx], removeItemByIdx ems idx)
                                            (Just km1, Just km2) -> case elemIndex km1 ims of
                                                                        Nothing -> case elemIndex km2 ims of
                                                                                        Nothing -> ([], ems)
                                                                                        Just idx2 -> ([ems !! idx2], removeItemByIdx ems idx2)
                                                                        Just idx1 -> let imsN = removeItemByIdx ims idx1
                                                                                         emsN = removeItemByIdx ems idx1
                                                                                     in  case elemIndex km2 imsN of
                                                                                            Nothing -> ([ems !! idx1], emsN)
                                                                                            Just idx2 -> ([ems !! idx1, emsN !! idx2], removeItemByIdx emsN idx2)

removeItemByIdx :: [a] -> Int -> [a]
removeItemByIdx xs idx = front ++ end
    where
        (front, _:end) = splitAt idx xs

updateKillerMoves :: Colour -> Transform -> (KillerMove, KillerMove) -> (KillerMove, KillerMove)
updateKillerMoves colour move kms = case kms of
                                    (Nothing, Nothing) -> (Just pos, Nothing)
                                    (Just a, Nothing) ->  (Just a, Just pos)
                                    (Nothing, Just a) -> (Just pos, Just a)
                                    (Just a, Just b) -> (Just pos, Just b)
    where
        pos = projectMove colour move


-- the enhancements applied here should include the: move ordering + k best pruning + killer moves for each layer
