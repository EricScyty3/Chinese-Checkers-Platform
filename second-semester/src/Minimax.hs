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
type MGameTreeStatus = (Int, -- current depth of the search tree
                        PlayerIndex, -- the root layer player's index, should be static
                        Board, -- the board state delivered from the parent
                        [[Pos]], -- the list of positions of the internal board for each player
                        Int, -- the total players, should be static
                        AlphaBeta,
                        TreeType, -- the choice of shallow minimax search, should be static
                        Int, -- the parameter of k-best pruning
                        KillerMoves -- a list of killer moves, in hashed form
                        )
type AlphaBeta = (Int, Int) -- the alpha and beta value, alhpa value represents the solution of Max node, and beta for Min node
type KillerMove = Maybe Transform
type KillerMoves = [(KillerMove, KillerMove)] -- the two last moves that were best or caused a cutoff at the current layer

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

testRun :: Int -> TreeType -> Transform
testRun depth treetype = let (tf, _) = mEvaluation (iStatus depth treetype) 0
                         in  tf

iStatus :: Int -> TreeType -> MGameTreeStatus
iStatus depth tt = (depth, 0, eboard, internalBoards, pn, (-999, 999), tt, 10, initialKillerMoves)
    where
        pn = 3
        eboard = eraseBoard (playerColourList pn)
        internalBoards = map (convertToInternalBoard eboard) (playerColourList pn)
        initialKillerMoves = replicate depth (Nothing, Nothing)


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

mEvaluation :: MGameTreeStatus -> PlayerIndex -> (Transform, Int)
mEvaluation st@(0, _, _, _, _, _, _, _, _)  pi = (defaultMove, nEvaluation st pi)
mEvaluation st@(depth, ri, eboard, iboard, pn, (alpha, beta), tt, k, kms) pi = 
                                                                      let ms = mplayerMovesList st pi -- provide a list of possible movements (pruned)
                                                                          nst = (depth, ri, eboard, iboard, pn, (alpha, beta), tt, k, kms) -- update the killer moves
                                                                      in  -- evaluate a list of movements, just like a list of nodes
                                                                          if ri /= pi then minEvaluation nst pi ms defaultMove
                                                                          else maxEvaluation nst pi ms defaultMove
defaultMove :: Transform
defaultMove = (U(-1,-1), U(-1,-1))

-- the evaluation here will first sync the game status based on the given movements, and then passes it to the next layer until reaching the bottom (set depth)  
-- while maintaining the best score and the corresponding movement                                                               
maxEvaluation :: MGameTreeStatus -> PlayerIndex -> [Transform] -> Transform -> (Transform, Int)
maxEvaluation (_, _, _, _, _, (alpha, _), _, _, _) _ [] bestMove = (bestMove, alpha) -- Max layer returns alpha value
maxEvaluation st@(depth, ri, board, pl, pn, ab@(alpha, beta), tt, k, ks) pi (m:ms) bestMove = 
                                                                                   let nib = flipBoard (pl !! pi) (projectMove (playerColour pi pn) m) -- edit the internal positions
                                                                                       npl = replace pi nib pl
                                                                                       neb = repaintPath board m -- edit the new board state
                                                                                       nst = (depth - 1, ri, neb, npl, pn, ab, tt, k, ks) -- retrieve new tree status
                                                                                       score = maximum $ treeSearch nst pi -- decide in which way keep (Paranoid or BRS style) digging down
                                                                                       -- and calculate the maximum score
                                                                                       (newtf, newAlpha) = if score >= alpha then (m, score) else (bestMove, alpha) -- update the alpha value and the best move so far
                                                                                   in  if newAlpha >= beta then (bestMove, beta) -- prune the branch if found a proof that the paranet won't choose this branch
                                                                                       else maxEvaluation (depth, ri, board, pl, pn, (newAlpha, beta), tt, k, ks) pi ms newtf -- get to the next movements provided in a list
-- similar to above but different in the return value
minEvaluation :: MGameTreeStatus -> PlayerIndex -> [Transform] -> Transform -> (Transform, Int)
minEvaluation (_, _, _, _, _, (_, beta), _, _, _) _ [] bestMove = (bestMove, beta) -- Min layer returns beta value
minEvaluation st@(depth, ri, board, pl, pn, ab@(alpha, beta), tt, k, ks) pi (m:ms) bestMove = 
                                                                                   let nib = flipBoard (pl !! pi) (projectMove (playerColour pi pn) m) 
                                                                                       npl = replace pi nib pl
                                                                                       neb = repaintPath board m 
                                                                                       nst = (depth - 1, ri, neb, npl, pn, ab, tt, k, ks) 
                                                                                       score = minimum $ treeSearch nst pi 
                                                                                       (newtf, newBeta) = if score <= beta then (m, score) else (bestMove, beta) -- update the beta value
                                                                                   in  if alpha >= newBeta then (bestMove, alpha)
                                                                                       else minEvaluation (depth, ri, board, pl, pn, (alpha, newBeta), tt, k, ks) pi ms newtf

-- the difference between two shallow minimax search: the Paranoid search follows the regular order base, while the BRS considers all players other than
-- the root as a layer, so it actually evaluates a list of boards from different players 
-- since it is not necessary to know about the best movement of the next layer, here it is ignored
treeSearch :: MGameTreeStatus -> PlayerIndex -> [Int]
treeSearch st@(0, _, _, _, _, _, _, _, _) pi = map snd [mEvaluation st pi]
treeSearch st@(_, ri, _, _, pn, _, Paranoid, _, _) pi = map snd [mEvaluation st (turnBase pn pi)]
treeSearch st@(_, ri, _, _, pn, _, BRS, _, _) pi = map (snd . mEvaluation st) (turnBaseBRS pn ri pi)
                                                     
-- when it comes to BRS, the search tree becomes different where the second layer's node is no longer one opponent, but all opponents
otherPlayers :: Int -> PlayerIndex -> [PlayerIndex]
otherPlayers pn ri = filter (/=ri) [0 .. pn - 1]
-- therefore, it needs a different turn switching mechanism
turnBaseBRS :: Int -> PlayerIndex -> PlayerIndex -> [PlayerIndex]
turnBaseBRS pn ri pi = if ri /= pi then [ri] else otherPlayers pn ri

-- evaluate the bottom node (where the depth is equal to 0) of the search tree
-- since the evaluation should be made based on the root's perspective, the evaluation of the other layer player is done by evaluating how the root player could benefit
nEvaluation :: MGameTreeStatus -> PlayerIndex -> Int
nEvaluation st@(_, ri, eboard, iboard, pn, _, _, _, _) pi = 
                                                 if ri == pi then boardEvaluation (iboard !! ri) -- for root, just simply measure its score
                                                 else let rolour = playerColour ri pn -- for other players, measure the possible root player's move could perform based on the changed board
                                                          rs = iboard !! ri
                                                          tfs = mplayerMovesList st ri -- the avaliable root's movements (pruned)
                                                          bs = map (flipBoard rs . projectMove rolour) tfs -- the resulting boards that could generated by the root player
                                                          scores = map boardEvaluation bs -- the corresponding score
                                                      in  maximum scores -- return the maximum one

-- the enhancements applied here should include the: move ordering + k best pruning + killer moves for each layer
-- given a certain piece's colour, return a list of avaliable movements (transforms) on the external board
mplayerMovesList :: MGameTreeStatus -> PlayerIndex -> [Transform]
mplayerMovesList (dp, _, eboard, iboard, pn, _, _, k, _) pi =
                                                    let colour = playerColour pi pn -- the colour of the player's piece
                                                        ps = (iboard !! pi) -- the internal positions of a player
                                                        bs = map (appendColour colour . reversion colour) ps -- project those position onto the external board
                                                        ds = evalState (do mapM destinationList bs) eboard -- and generate movements based on the external board state
                                                        ms = pairArrange bs ds -- zip the start and the end, ensuring that those are listed in pairs
                                                        -- before sending to the layer, k-best pruning is implemented here
                                                        orderedList = moveOrder colour ms
                                                        -- (nkp, reorederedList) = killerMoveTest (ks !! currentIdx) orderedList -- since the depth is decrementing, from 2 to 0
                                                    in  -- (replace currentIdx nkp ks, take k reorederedList)
                                                        take k orderedList
                                                        
    -- where
    --     currentIdx = length ks - dp

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

inc :: Int -> State Int ()
inc x = do gg <- get
           put (gg + x)

{-
-- search if there exist any killer moves at the current layer of minimax search
-- if it does, then we have to put it to the front of the sorted list such that them will be tried first
-- normally, the killer moves are specifically existing on certain ply, therefore, each ply should store its own killer moves
-- besides, since the killer moves are specific for certain layer, if wanting to reuse those moves, it will be better to modify it for the next iteration, as the game progresses also push the layer of the search
-- since the target of killer moves is to save time, it should be tried first without considering any later computation
killerMoveTest :: (KillerMove, KillerMove) -> [Transform] -> ((KillerMove, KillerMove), [Transform]) 
-- besides of searching for the existing killer moves, it also reorder the killer moves such that the one less recent being applied will be placed first
killerMoveTest pair@(Nothing, Nothing) ms = (pair, ms)
killerMoveTest pair@(Just km, Nothing) ms = case elemIndex km ms of
                                            Nothing ->  (pair, ms)
                                            Just idx -> ((Nothing, Just km), switchItem2Front ms idx)
killerMoveTest pair@(Nothing, Just km) ms = case elemIndex km ms of
                                            Nothing ->  ((Just km, Nothing), ms)
                                            Just idx -> (pair, switchItem2Front ms idx)
killerMoveTest pair@(Just k1, Just k2) ms = case elemIndex k1 ms of
                                            Nothing -> case elemIndex k2 ms of
                                                            Nothing ->   (pair, ms)
                                                            Just idx2 -> (pair, switchItem2Front ms idx2)
                                            Just idx1 -> let nms = switchItem2Front ms idx1
                                                         in  case elemIndex k2 ms of
                                                                Nothing ->   ((Just k2, Just k1), nms)
                                                                Just idx2 -> (pair, switchItem2Front nms idx2)
switchItem2Front :: [a] -> Int -> [a]
switchItem2Front xs idx = item:front ++ end
    where
        (front, item:end) = splitAt idx xs

updateKillerMoves :: Transform -> (KillerMove, KillerMove) -> (KillerMove, KillerMove)
updateKillerMoves move kms = case kms of
                                (Nothing, Nothing) -> (Just move, Nothing)
                                (Just a, Nothing) ->  (Just a, Just move)
                                (Nothing, Just a) -> (Just move, Just a)
                                (Just a, Just b) -> (Just move, Just b) 
                                -- since the update will always replace the first one, it's necessary to switch the order if one did not cause a recent cutoff 
-}

