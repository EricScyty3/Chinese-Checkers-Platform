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
import GHC.Real
import Data.List (elemIndices)
import System.Environment

-- the node is divided into two groups, Max for root layer and the rest is Min 
-- the search tree is also divided into two groups, the paranoid and BRS forms
data TreeType = Paranoid | BRS deriving (Eq, Show)
-- the new tree status with less information to consider
type MGameTreeStatus = (PlayerIndex, -- the root layer player's index
                        PlayerIndex, -- current player of the turn
                        Board, -- the board state delivered from the parent
                        [[Pos]], -- the list of positions of the internal board for each player
                        Int -- the total players
                        )

type AlphaBeta = (Rational, Rational) -- the alpha and beta value, alhpa value represents the solution of Max node, and beta for Min node

main = do arg <- getArgs
          start <- getCurrentTime
          print $ startFromRoot (read $ head arg) iStatus
          end <- getCurrentTime
          print $ diffUTCTime end start

iStatus :: MGameTreeStatus
iStatus = (0, 0, eboard, internalBoards, pn)
    where
        pn = 3
        eboard = eraseBoard (playerColourList pn)
        internalBoards = initialInternalBoard eboard pn


startFromRoot :: Int -> MGameTreeStatus -> Transform
startFromRoot depth st@(ri, pi, board, pl, pn) = let tfs = mplayerMovesList (playerColour ri pn) (pl !! ri) board
                                                     scores = map (mEvaluation depth st) tfs
                                                 in  tfs !! randomMaxIdx scores

-- start the search, layer by layer until reaching certain depth
-- evaluate a node (move)
mEvaluation :: Int -> MGameTreeStatus -> Transform -> Int
mEvaluation depth (ri, pi, board, pl, pn) tf =
                        let nboard = repaintPath board tf -- first render the change to the board
                            colour = playerColour pi pn
                            ps = pl !! pi -- then update the internal state
                            nps = flipBoard ps (projectMove colour tf)
                            npl = replace pi nps pl
                            -- after that, start investigating the children nodes 
                            -- but before really dig into the board, first comfirm the win state
                            winIdx = checkPlayersWinState pn nboard
                        in  if winIdx /= -1 then (if winIdx == ri then 28 else 0) 
                            else dEvaluation depth (ri, turnBase pn pi, nboard, npl, pn)

-- the evaluation is based on the perspective of root player
-- evaluating a board state
dEvaluation :: Int -> MGameTreeStatus -> Int
dEvaluation 0 (ri, pi, board, pl, pn) = 
                                    if ri == pi then boardEvaluation (pl !! ri) -- measuring root layer's score
                                    else let rolour = playerColour ri pn -- measuring the possible root player's move could perform
                                             ps = pl !! ri
                                             tfs = mplayerMovesList rolour ps board
                                             nps = map (flipBoard ps . projectMove rolour) tfs
                                             scores = map boardEvaluation nps
                                         in  maximum scores
dEvaluation depth (ri, pi, board, pl, pn) = 
                                        let tfs = mplayerMovesList (playerColour pi pn) (pl !! pi) board
                                            scores = map (mEvaluation (depth - 1) (ri, pi, board, pl, pn)) tfs
                                        in  if ri /= pi then minimum scores else maximum scores

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

