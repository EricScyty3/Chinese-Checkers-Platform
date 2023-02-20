module Minimax where
-- the module of the shallow minimax search: Paranoid and BRS
import Board
import Control.Monad.State
import GameTree
import Configuration (boardEvaluation)
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
                        PlayerIndex, -- current player of the turn
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
          start <- getCurrentTime
          print $ mEvaluation (read $ head arg) iStatus
        --   a <- startFromRoot (read $ head arg) iStatus
        --   print a
          end <- getCurrentTime
          print $ diffUTCTime end start

iStatus :: MGameTreeStatus
iStatus = (0, 0, eboard, internalBoards, pn, (-999, 999))
    where
        pn = 3
        eboard = eraseBoard (playerColourList pn)
        internalBoards = initialInternalBoard eboard pn

-- getNodeType :: MGameTreeStatus -> NodeType
-- getNodeType (ri, pi, _, _, _, _) = if ri == pi then Max else Min

-- the pruning is available based on evaluating the nodes below
-- normally, when a Min node's value is larger than any parent(Max)'s alpha value, that branch can be pruned
-- on the other hand, when a Max node's value is less than any parent(Min)'s beta value, the pruning can be applied
-- however, in a multi-player version, the switch of Max and Min nodes are not done layer by layer, 
-- for instance, a paranoid tree is having one Max layer and many of Min layers which makes it complicated than regular pruning

--                  root (Max)     Given board state
--                 /    \
--               Min    Min        The resulting board states from the root player
--               /        \
--            Bottom     Bottom    The resulting board state made by the first encountering player (one of the opponents) from the parent board
--                                 at this layer, the evaluation is located

mEvaluation 0 st = nEvaluation st
mEvaluation depth st@(ri, pi, board, ps, pn, (alpha, beta)) = let ms = mplayerMovesList (playerColour pi pn) (ps !! pi) board
                                                              in  if ri /= pi then minEvaluation depth st ms
                                                                  else maxEvaluation depth st ms
maxEvaluation :: Int -> MGameTreeStatus -> [Transform] -> Int
maxEvaluation _ (_, _, _, _, _, (alpha, _)) [] = alpha
maxEvaluation depth st@(ri, pi, board, ps, pn, (alpha, beta)) (m:ms) =
                                                                      let npi = turnBase pn pi
                                                                          nib = flipBoard (ps !! pi) (projectMove (playerColour pi pn) m)
                                                                          nps = replace pi nib ps
                                                                          neb = npi `par` nps `pseq` repaintPath board m
                                                                          nst = (ri, npi, neb, nps, pn, (alpha, beta))
                                                                          score = mEvaluation (depth - 1) nst
                                                                          newAlpha = maximum [alpha, score]
                                                                      in  if newAlpha > beta then beta
                                                                          else maxEvaluation depth (ri, pi, board, ps, pn, (newAlpha, beta)) ms
minEvaluation :: Int -> MGameTreeStatus -> [Transform] -> Int
minEvaluation _ (_, _, _, _, _, (_, beta)) [] = beta
minEvaluation depth st@(ri, pi, board, ps, pn, (alpha, beta)) (m:ms) =
                                                                      let npi = turnBase pn pi
                                                                          nib = flipBoard (ps !! pi) (projectMove (playerColour pi pn) m)
                                                                          nps = replace pi nib ps
                                                                          neb = npi `par` nps `pseq` repaintPath board m
                                                                          nst = (ri, npi, neb, nps, pn, (alpha, beta))
                                                                          score = mEvaluation (depth - 1) nst
                                                                          newBeta = minimum [beta, score]
                                                                      in  if alpha > newBeta then alpha
                                                                          else minEvaluation depth (ri, pi, board, ps, pn, (alpha, newBeta)) ms

-- evaluate the bottom node (where the depth is equal to 0) of the search tree
nEvaluation :: MGameTreeStatus -> Int
nEvaluation (ri, pi, board, pl, pn, _) = if ri == pi then boardEvaluation (pl !! ri) -- measuring root layer's score
                                         else let rolour = playerColour ri pn -- measuring the possible root player's move could perform
                                                  rs = pl !! ri
                                                  tfs = mplayerMovesList rolour rs board
                                                  ns = map (flipBoard rs . projectMove rolour) tfs
                                                  scores = map boardEvaluation ns
                                              in  maximum scores
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
-- given a certain piece's colour, return a list of avaliable movements (transforms)
mplayerMovesList :: Colour -> [Pos] -> Board -> [Transform]
mplayerMovesList colour ps board = let bs = map (appendColour colour . reversion colour) ps
                                       ds = evalState (do mapM destinationList bs) board
                                   in  pairArrange bs ds

-- selecting randomly if there exist multiple maximum elements in a list
randomMaxIdx :: Ord a => [a] -> Int
randomMaxIdx []  = error "Selection: no node for selecting"
randomMaxIdx xs  = let is = elemIndices (maximum xs) xs
                   in  if length is == 1 then head is
                       else let ri = randomMove (length is)  -- random index of the maximum values' indices
                            in  is !! ri -- return the maximum value's index for selecting

-- selecting randomly if there exist multiple minimum elements in a list
randomMinIdx :: Ord a => [a] -> Int
randomMinIdx []  = error "Selection: no node for selecting"
randomMinIdx xs  = let is = elemIndices (minimum xs) xs
                   in  if length is == 1 then head is
                       else let ri = randomMove (length is)  -- random index of the minimum values' indices
                            in  is !! ri -- return the minimum value's index for selecting

