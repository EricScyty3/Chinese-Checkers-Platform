module GameTree where

import Data.List
import Data.Maybe
import Zobrist
import ShortestPath


-- Normal MCTS is divided into four phases: selection, expansion, playout and backpropagation
-- the first phase is to select one resulting board with the maximum profit

-- the whole structure requires a state monad that stores the constructed game tree
-- the score is calculated based on different strategies: UCT for MCTS selection or move-based evaluation of minimax algorithm
type WinRate = Double
type Score = Int
type Counts = Int
data GameTree = GLeaf Score OccupiedBoard | GNode Score OccupiedBoard [GameTree] deriving (Eq, Show)
-- the game tree contains two main types: leaf and internal node with at least one child

estimate :: OccupiedBoard -> Int
estimate = centroid

-- requires average score/win rate, 
-- uct :: WinRate -> Counts -> Counts -> 

-- initially setup a node (leaf)
makeNode :: OccupiedBoard -> GameTree
makeNode = GLeaf 0

getChildren :: GameTree -> [GameTree]
getChildren (GLeaf _ _) = []
getChildren (GNode _ _ t) = t

getBoard :: GameTree -> OccupiedBoard
getBoard (GLeaf _ b) = b
getBoard (GNode _ b _) = b

getScore :: GameTree -> Int
getScore (GLeaf s _) = s
getScore (GNode s _ _) = s

-- might have different definitions of profits
selectNode :: GameTree -> GameTree
selectNode t@(GLeaf _ b) = let newNode = expandNode t
                           in  selectNode newNode
selectNode (GNode _ _ ts) = let maxScore = maximum (map getScore ts)
                                idx = head (elemIndices maxScore (map getScore ts))
                            in  ts !! idx
-- might have restriction of expanded nodes
expandNode :: GameTree -> GameTree
expandNode (GLeaf score board) = let boardCentroid = centroid board
                                     moveList = dListForBoard board
                                     boardList = flipLists board boardCentroid moveList -- [(O, C)]
                                     frontwardList = filter ((>= boardCentroid) . snd) boardList -- remove the non-frontward moves
                                 in  GNode score board (map (makeNode . fst) frontwardList)
expandNode (GNode score board t) = GNode score board t
-- random simulation, but could add some strategies
-- playoutNode :: GameTree -> Int
-- playoutNode (GNode s _ ts) = s
-- playoutNode (GLeaf )
