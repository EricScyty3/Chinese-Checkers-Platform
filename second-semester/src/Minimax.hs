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
                        TreeType -- the choice of shallow minimax search, should be static
                        )
type AlphaBeta = (Int, Int) -- the alpha and beta value, alhpa value represents the solution of Max node, and beta for Min node
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
{-
iStatus :: TreeType -> MGameTreeStatus
iStatus tt = (0, eboard, internalBoards, pn, (-999, 999), tt)
    where
        pn = 3
        eboard = eraseBoard (playerColourList pn)
        internalBoards = initialInternalBoard eboard pn
-}

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
mEvaluation depth st@(ri, board, pl, pn, (alpha, beta), _) pi = let ms = mplayerMovesList st pi -- provide a list of possible movements
                                                                in  -- evaluate a list of movements, just like a list of nodes
                                                                    if ri /= pi then minEvaluation depth st pi ms (U(-1,-1), U(-1,-1))
                                                                    else maxEvaluation depth st pi ms (U(-1,-1), U(-1,-1))

-- the evaluation here will first sync the game status based on the given movements, and then passes it to the next layer until reaching the bottom (set depth)  
-- while maintaining the best score and the corresponding movement                                                               
maxEvaluation :: Int -> MGameTreeStatus -> PlayerIndex -> [Transform] -> Transform -> (Transform, Int)
maxEvaluation _ (_, _, _, _, (alpha, _), _) _ [] tf = (tf, alpha) -- Max layer returns alpha value
maxEvaluation depth st@(ri, board, pl, pn, ab@(alpha, beta), tt) pi (m:ms) tf = let nib = flipBoard (pl !! pi) (projectMove (playerColour pi pn) m) -- edit the internal positions
                                                                                    npl = replace pi nib pl
                                                                                    neb = repaintPath board m -- edit the new board state
                                                                                    newDepth = depth - 1
                                                                                    nst = (ri, neb, npl, pn, ab, tt) -- retrieve new tree status
                                                                                    score = maximum $ treeSearch newDepth nst pi -- decide in which way keep (Paranoid or BRS style) digging down
                                                                                    -- and calculate the maximum score
                                                                                    (newtf, newAlpha) = if score >= alpha then (m, score) else (tf, alpha) -- update the alpha value and the best move so far
                                                                                in  if newAlpha >= beta then (tf, beta) -- prune the branch if found a proof that the paranet won't choose this branch
                                                                                    else maxEvaluation depth (ri, board, pl, pn, (newAlpha, beta), tt) pi ms newtf -- get to the next movements provided in a list
-- similar to above but different in the return value
minEvaluation :: Int -> MGameTreeStatus -> PlayerIndex -> [Transform] -> Transform -> (Transform, Int)
minEvaluation _ (_, _, _, _, (_, beta), _) _ [] tf = (tf, beta) -- Min layer returns beta value
minEvaluation depth st@(ri, board, pl, pn, ab@(alpha, beta), tt) pi (m:ms) tf = let nib = flipBoard (pl !! pi) (projectMove (playerColour pi pn) m) 
                                                                                    npl = replace pi nib pl
                                                                                    neb = repaintPath board m 
                                                                                    newDepth = depth - 1
                                                                                    nst = (ri, neb, npl, pn, ab, tt) 
                                                                                    score = minimum $ treeSearch newDepth nst pi 
                                                                                    (newtf, newBeta) = if score <= beta then (m, score) else (tf, beta) -- update the beta value
                                                                                in  if alpha >= newBeta then (tf, alpha)
                                                                                    else minEvaluation depth (ri, board, pl, pn, (alpha, newBeta), tt) pi ms newtf

-- the difference between two shallow minimax search: the Paranoid search follows the regular order base, while the BRS considers all players other than
-- the root as a layer, so it actually evaluates a list of boards from different players 
-- since it is not necessary to know about the best movement of the next layer, here it is ignored
treeSearch :: Int -> MGameTreeStatus -> PlayerIndex -> [Int]
treeSearch 0 st pi = map snd [mEvaluation 0 st pi]
treeSearch depth st@(ri, _, _, pn, _, Paranoid) pi = map snd [mEvaluation depth st (turnBase pn pi)]
treeSearch depth st@(ri, _, _, pn, _, BRS) pi = map (snd . mEvaluation depth st) (turnBaseBRS pn ri pi)
                                                     
-- when it comes to BRS, the search tree becomes different where the second layer's node is no longer one opponent, but all opponents
otherPlayers :: Int -> PlayerIndex -> [PlayerIndex]
otherPlayers pn ri = filter (/=ri) [0 .. pn - 1]
-- therefore, it needs a different turn switching mechanism
turnBaseBRS :: Int -> PlayerIndex -> PlayerIndex -> [PlayerIndex]
turnBaseBRS pn ri pi = if ri /= pi then [ri] else otherPlayers pn ri

-- evaluate the bottom node (where the depth is equal to 0) of the search tree
-- since the evaluation should be made based on the root's perspective, the evaluation of the other layer player is done by evaluating how the root player could benefit
nEvaluation :: MGameTreeStatus -> PlayerIndex -> Int
nEvaluation st@(ri, board, pl, pn, _, _) pi = if ri == pi then boardEvaluation (pl !! ri) -- for root, just simply measure its score
                                              else let rolour = playerColour ri pn -- for other players, measure the possible root player's move could perform based on the changed board
                                                       rs = pl !! ri
                                                       tfs = mplayerMovesList st ri -- the avaliable root's movements
                                                       bs = map (flipBoard rs . projectMove rolour) tfs -- the resulting boards that could generated by the root player
                                                       scores = map boardEvaluation bs -- the corresponding score
                                                   in  maximum scores -- return the maximum one

-- given a certain piece's colour, return a list of avaliable movements (transforms) on the external board
mplayerMovesList :: MGameTreeStatus -> PlayerIndex -> [Transform]
mplayerMovesList (ri, board, pl, pn, ab, _) pi = let colour = playerColour pi pn -- the colour of the player's piece
                                                     ps = (pl !! pi) -- the internal positions of a player
                                                     bs = map (appendColour colour . reversion colour) ps -- project those position onto the external board
                                                     ds = evalState (do mapM destinationList bs) board -- and generate movements based on the external board state
                                                 in  pairArrange bs ds -- zip the start and the end, ensuring that those are listed in pairs

