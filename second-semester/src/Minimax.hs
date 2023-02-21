module Minimax where
-- the module of the shallow minimax search: Paranoid and BRS
import Board
import Control.Monad.State
import GameTree
import Configuration (boardEvaluation, lookupTable)
import Control.Parallel
import Zobrist
import MCTS
import Data.Time
import Data.List (elemIndices)
import System.Environment
import qualified Control.Monad

-- the node is divided into two groups, Max for root layer and the rest is Min 
-- the search tree is also divided into two groups, the paranoid and BRS forms
data TreeType = Paranoid | BRS deriving (Eq, Show)
data NodeType = Max | Min deriving (Eq, Show)
-- the new tree status with less information to consider
type MGameTreeStatus = (PlayerIndex, -- the root layer player's index
                        Board, -- the board state delivered from the parent
                        [[Pos]], -- the list of positions of the internal board for each player
                        Int, -- the total players
                        AlphaBeta
                        )

type AlphaBeta = (Int, Int) -- the alpha and beta value, alhpa value represents the solution of Max node, and beta for Min node
-- the alpha and beta pair is not consistent throughout the search tree, it might be switched, for instance, 
-- the maximum value of the subtree could be the minimum value of its parent

-- ghc -main-is Minimax Minimax.hs -O2 -outputdir dist
main = do arg <- getArgs
          start <- lookupTable `pseq` getCurrentTime
          print $ mEvaluation (read $ head arg) iStatus 0
        --   a <- startFromRoot (read $ head arg) iStatus
        --   print a
          end <- getCurrentTime
          print $ diffUTCTime end start

iStatus :: MGameTreeStatus
iStatus = (0, eboard, internalBoards, pn, (-999, 999))
    where
        pn = 3
        eboard = eraseBoard (playerColourList pn)
        internalBoards = initialInternalBoard eboard pn

-- getNodeType :: MGameTreeStatus -> NodeType
-- getNodeType (ri, pi, _, _, _, _) = if ri == pi then Max else Min

-- the pruning is available based on evaluating the nodes below
-- normally, when a Min node's (beta) value is larger than any parent(Max)'s alpha value, that branch can be pruned
-- on the other hand, when a Max node's (alpha) value is less than any parent(Min)'s beta value, the pruning can be applied
-- however, in a multi-player version, the switch of Max and Min nodes are not done layer by layer, 
-- for instance, a paranoid tree is having one Max layer and many of Min layers which makes it complicated than regular pruning
-- but overall, the return value is consistent, for instance, the Max node always returns alpha value while Min returns beta

--                  root           Given board state
--                 /    \
--                A      B        The new board states made by the root player
--               / \    / \
--              C   D  E   F      The new board states made by the first encountering player (one of the opponents)

mEvaluation :: Int -> MGameTreeStatus -> PlayerIndex -> Int
mEvaluation 0 st pi = nEvaluation st pi
mEvaluation depth st@(ri, board, pl, pn, (alpha, beta)) pi = let ms = mplayerMovesList st pi -- provide a list of possible movements
                                                             in  if ri /= pi then minEvaluation depth st pi ms 
                                                                 else maxEvaluation depth st pi ms

-- the evaluation here will first sync the game status based on the given movement, and then pass it to the next layer until reaching the bottom (set depth)                                                                 
maxEvaluation :: Int -> MGameTreeStatus -> PlayerIndex -> [Transform] -> Int
maxEvaluation _ (_, _, _, _, (alpha, _)) _ [] = alpha
maxEvaluation depth st@(ri, board, pl, pn, ab@(alpha, beta)) pi (m:ms) = let nib = flipBoard (pl !! pi) (projectMove (playerColour pi pn) m)
                                                                             npl = replace pi nib pl
                                                                             neb = repaintPath board m
                                                                             newDepth = depth - 1
                                                                             nst = (ri, neb, npl, pn, ab)
                                                                             {-score = if newDepth == 0 then mEvaluation newDepth (ri, neb, npl, pn, ab) pi
                                                                                     else mEvaluation newDepth (ri, neb, npl, pn, ab) (turnBase pn pi)
                                                                             -}
                                                                             scores = treeSearch BRS newDepth nst pi
                                                                             newAlpha = maximum (alpha:scores)
                                                                         in  if newAlpha >= beta then beta
                                                                             else maxEvaluation depth (ri, board, pl, pn, (newAlpha, beta)) pi ms
minEvaluation :: Int -> MGameTreeStatus -> PlayerIndex -> [Transform] -> Int
minEvaluation _ (_, _, _, _, (_, beta)) _ [] = beta
minEvaluation depth st@(ri, board, pl, pn, ab@(alpha, beta)) pi (m:ms) = let nib = flipBoard (pl !! pi) (projectMove (playerColour pi pn) m)
                                                                             npl = replace pi nib pl
                                                                             neb = repaintPath board m
                                                                             newDepth = depth - 1
                                                                             nst = (ri, neb, npl, pn, ab)
                                                                             {-score = if newDepth == 0 then mEvaluation newDepth (ri, neb, npl, pn, ab) pi
                                                                                     else mEvaluation newDepth (ri, neb, npl, pn, ab) (turnBase pn pi)-}
                                                                             scores = treeSearch BRS newDepth nst pi
                                                                             newBeta = minimum (beta:scores)
                                                                         in  if alpha >= newBeta then alpha
                                                                             else minEvaluation depth (ri, board, pl, pn, (alpha, newBeta)) pi ms

treeSearch :: TreeType -> Int -> MGameTreeStatus -> PlayerIndex -> [Int]
treeSearch _ 0 st pi = [mEvaluation 0 st pi]
treeSearch Paranoid depth st@(ri, board, pl, pn, ab) pi = [mEvaluation depth st (turnBase pn pi)]
treeSearch BRS depth st@(ri, board, pl, pn, ab) pi = map (mEvaluation depth st) (turnBaseBRS pn ri pi)
                                                     
-- when it comes to BRS, the search tree becomes different where the second layer's node is no longer one opponent, but all opponents
otherPlayers :: Int -> PlayerIndex -> [PlayerIndex]
otherPlayers pn ri = filter (/=ri) [0 .. pn - 1]
-- a different turn switching mechanism
turnBaseBRS :: Int -> PlayerIndex -> PlayerIndex -> [PlayerIndex]
turnBaseBRS pn ri pi = if ri /= pi then [ri] else otherPlayers pn ri
{-
startFromRoot :: Int -> MGameTreeStatus -> IO Int
startFromRoot depth st = dEvaluation depth st
-- basically update the board state
-- mEvaluation :: Int -> MGameTreeStatus -> Transform -> Int
mEvaluation depth (ri, pi, board, pl, pn, ab) tf =
                        let nboard = repaintPath board tf -- first render the change to the board
                            colour = playerColour pi pn
                            ps = pl !! pi -- then update the internal state
                            nps = flipBoard ps (projectMove colour tf)
                            npl = replace pi nps pl
                            -- after that, start investigating the children nodes 
                            -- but before really dig into the board, first comfirm the win state
                            winIdx = checkPlayersWinState pn nboard
                        in  if winIdx /= -1 then (if winIdx == ri then return 28 else return 0)
                            else do dEvaluation depth (ri, turnBase pn pi, nboard, npl, pn, ab)

-- the evaluation is based on the perspective of root player
-- evaluating a board state
-- dEvaluation :: Int -> MGameTreeStatus -> [[Int]]
dEvaluation 0 st = do -- print ("B" ++ show (nEvaluation st))
                      return (nEvaluation st)
dEvaluation depth (ri, pi, board, pl, pn, ab) = do let tfs = mplayerMovesList (playerColour pi pn) (pl !! pi) board
                                                   scores <- mapM (mEvaluation (depth - 1) (ri, pi, board, pl, pn, ab)) tfs
                                                   let tempScore = if ri /= pi then minimum scores else maximum scores
                                                   --print (show depth ++ " " ++ show scores ++ " " ++ show tempScore)
                                                   return tempScore
-}

-- evaluate the bottom node (where the depth is equal to 0) of the search tree
nEvaluation :: MGameTreeStatus -> PlayerIndex -> Int
nEvaluation st@(ri, board, pl, pn, _) pi = if ri == pi then boardEvaluation (pl !! ri) -- measuring root layer's score
                                           else let rolour = playerColour ri pn -- measuring the possible root player's move could perform
                                                    rs = pl !! ri
                                                    tfs = mplayerMovesList st ri
                                                    ns = map (flipBoard rs . projectMove rolour) tfs
                                                    scores = map boardEvaluation ns
                                                in  maximum scores

-- given a certain piece's colour, return a list of avaliable movements (transforms)
mplayerMovesList :: MGameTreeStatus -> PlayerIndex -> [Transform]
mplayerMovesList (ri, board, pl, pn, ab) pi = let colour = playerColour pi pn
                                                  ps = (pl !! pi)
                                                  bs = map (appendColour colour . reversion colour) ps
                                                  ds = evalState (do mapM destinationList bs) board
                                              in  pairArrange bs ds

