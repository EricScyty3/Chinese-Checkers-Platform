module GameTree where

import Data.List
import Data.Maybe
import Zobrist
import ShortestPath
import RBTree
import Board
import State


type Wins    = Int
type Visits  = Int
type Transform = (Pos, Pos) -- the conversion between two adjacent boards, known the positions pair could transform the board state as well as its hash

-- the game tree contains two main types: leaf and internal node with at least one child
-- the root node can be represented individually as it does not need the board value
data GameTree = GRoot (OccupiedBoard, Hash) [GameTree] |
                GLeaf (OccupiedBoard, Hash) Transform Wins Visits|
                GNode (OccupiedBoard, Hash) Transform Wins Visits [GameTree]
                deriving (Eq, Show)

type HistoryTree = RBTree
type Trace = [Hash]

push :: Hash -> Trace -> Trace
push x xs = x:xs

pop :: Trace -> (Hash, Trace)
pop xs = (last xs, init xs)

hashBoard :: OccupiedBoard -> Hash
hashBoard b = hashState b randomBoardState

expandHashBoard :: Hash -> (Pos, Pos) -> Int
expandHashBoard h (f, t) = hashChange f t h

getChildren :: GameTree -> [GameTree]
getChildren (GRoot _ t) = t
getChildren (GNode _ _ _ _ t) = t
getChildren _ = []

getBoardState :: GameTree -> (OccupiedBoard, Hash)
getBoardState (GRoot bs _) = bs
getBoardState (GLeaf bs _ _ _) = bs
getBoardState (GNode bs _ _ _ _) = bs

getWins :: GameTree -> Wins
getWins (GLeaf _ _ w _) = w
getWins (GNode _ _ w _ _) = w
getWins _ = 0

getVisits :: GameTree -> Visits
getVisits (GLeaf _ _ _ v) = v
getVisits (GNode _ _ _ v _) = v
getVisits _ = 0

maxIndex :: [Int] -> Int
maxIndex ns = head (elemIndices (maximum ns) ns)

-- edit the wins and visits for a node
editNode :: (Wins, Visits) -> GameTree -> GameTree
editNode _ r@(GRoot _ _) = r
editNode (w, v) (GLeaf b ps _ _) = GLeaf b ps w v
editNode (w, v) (GNode b ps _ _ ts) = GNode b ps w v ts


updateGameTreeWithTrace :: (Int, Int) -> Trace -> GameTree -> GameTree -- done by updating the node's children
updateGameTreeWithTrace (a, b) xs (GRoot s ts) = let (h, ys) = pop xs
                                                     idx = fromMaybe 0 (elemIndex h (map (snd . getBoardState) ts))
                                                     node = ts !! idx
                                                     newN = updateGameTreeWithTrace (a, b) ys (editNode (getWins node + a, getVisits node + b) node) 
                                                     newL = replace idx newN ts
                                                 in  GRoot s newL



                                           
