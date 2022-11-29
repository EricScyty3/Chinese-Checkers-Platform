module GameTree where

import Data.List
import Data.Maybe
import Zobrist
import Board

type Wins = Int
type PlayerIndex = Int
type BoardIndex = Int
type Transform = (BoardType, BoardType) -- the conversion between two adjacent boards, known the positions pair could transform the board state as well as its hash
-- the game tree contains two main types: leaf and internal node with at least one child
-- the root node can be represented individually as it does not need the board value
-- normally, each node should hold the overall board of the game and a tuple of size N = players number representing the scroe for each player
-- but the board could be transformed into a square board for each specific player based on the projection
-- and the piece change is delivered through projecting to the main board system and to another sub-board system 

data GameTree = GRoot BoardIndex Board [Wins] [GameTree] |
                GLeaf BoardIndex Board Transform [Wins]  |
                GNode BoardIndex Board Transform [Wins] [GameTree]
                deriving (Eq, Show)

printGameTree :: GameTree -> IO ()
printGameTree gt = do printEoard (getBoard gt)
                      putStrLn ("Index: " ++ show (getBoardIndex gt))
                      putStrLn  ("Win List: " ++ show(getWins gt))

getBoardIndex :: GameTree -> BoardIndex
getBoardIndex (GRoot idx _ _ _) = idx
getBoardIndex (GNode idx _ _ _ _) = idx
getBoardIndex (GLeaf idx _ _ _) = idx

getBoard :: GameTree -> Board
getBoard (GRoot _ b _ _) = b
getBoard (GNode _ b _ _ _) = b
getBoard (GLeaf _ b _ _) = b

getChildren :: GameTree -> [GameTree]
getChildren (GRoot _ _ _ ts) = ts
getChildren (GNode _ _ _ _ ts) = ts
getChildren _ = []

getWins :: GameTree -> [Wins]
getWins (GRoot _ _ ws _) = ws
getWins (GNode _ _ _ ws _) = ws
getWins (GLeaf _ _ _ ws) = ws

getVisits :: GameTree -> Int
getVisits gt = sum (getWins gt) -- the sum of the win counts equals to the total visits of that node

getPlayers :: GameTree -> Int
getPlayers gt = length (getWins gt) -- the length of win list equals to the total players number

getTransform :: GameTree -> Transform
getTransform (GLeaf _ _ ft _) = ft
getTransform (GNode _ _ ft _ _) = ft
getTransform r@GRoot {} = (U(-1, -1), U(-1, -1))

-- transform the display board into occupied board based on piece's colour 
projectCOB :: Colour -> Board -> OccupiedBoard
projectCOB colour eboard = let ps = map getPos (findColouredPieces colour eboard)
                               cs = map (projection colour) ps
                           in  fillBoard cs empty
    where
        fillBoard [] b = b
        fillBoard (p:ps) b = fillBoard ps (replace2 p 1 b)

--- provide the expanded board based on given board state and player's colour
expandingBoards :: Colour -> Board -> [(Board, Transform)]
expandingBoards colour eboard = let ml = colouredMoveList colour eboard
                                in  repaintPaths eboard colour ml

-- render a list of movements and return a list of boards and the transformed position pairs
repaintPaths :: Board -> Colour -> [(BoardType, [BoardType])] -> [(Board, Transform)]
repaintPaths _ _ [] = []
repaintPaths eboard colour (x:xs) = let (b, bs) = x
                                    in  map (repaintPath colour eboard b) bs ++ repaintPaths eboard colour xs
    where
        repaintPath :: Colour -> Board -> BoardType -> BoardType -> (Board, Transform)
        repaintPath c eBoard start end  = let tempBoard = changeBoardElement erase start eBoard
                                          in  (changeBoardElement (repaint c) end tempBoard, (start, end))

-- find the pieces' positions on the board based on the colour
findColouredPieces :: Colour -> Board -> [BoardType]
findColouredPieces c = concatMap (findColouredPieces' c)
    where
        findColouredPieces' :: Colour -> [BoardType] -> [BoardType]
        findColouredPieces' _ [] = []
        findColouredPieces' c (x:xs) = if compareColour x c then x : findColouredPieces' c xs else findColouredPieces' c xs

-- provide the available pieces and their movement pairs
colouredMoveList :: Colour -> Board -> [(BoardType, [BoardType])]
colouredMoveList colour eboard = let bs = findColouredPieces colour eboard
                                 in  zip bs (map (destinationListFilter eboard) bs) -- zip the from and to destinations

-- return the current player's colour based on the number of players
currentPlayerColour :: PlayerIndex -> Int -> Colour
currentPlayerColour idx number
    | number == 2 = twoPlayersSet !! idx
    | number == 3 = threePlayersSet !! idx
    | number == 4 = fourPlayersSet !! idx
    | otherwise = sixPlayersSet !! idx

-- edit the wins and visits for a node
editNodeValue :: PlayerIndex -> GameTree -> GameTree
editNodeValue pi (GRoot i b ws ts) = let nws = replace pi ((ws!!pi)+1) ws
                                     in  GRoot i b nws ts
editNodeValue pi (GLeaf i b ft ws) = let nws = replace pi ((ws!!pi)+1) ws
                                     in  GLeaf i b ft nws
editNodeValue pi (GNode i b ft ws ts) = let nws = replace pi ((ws!!pi)+1) ws
                                        in  GNode i b ft nws ts

editNodeChildren :: [GameTree] -> GameTree -> GameTree
editNodeChildren [] t = t
editNodeChildren ts (GRoot i b ws _) = GRoot i b ws ts
editNodeChildren ts (GNode i b ft ws _) = GNode i b ft ws ts
editNodeChildren ts (GLeaf i b ft ws) = GNode i b ft ws ts

-- search for a certain child node of the current node based on the index
findChildNode :: BoardIndex -> [GameTree] -> (GameTree, Int)
findChildNode i ts = findChildNode' i ts 0
    where
        findChildNode' :: BoardIndex -> [GameTree] -> Int -> (GameTree, Int)
        findChildNode' _ [] _ = error "Cannot find the backpropagated node"
        findChildNode' i (t:ts) idx = if getBoardIndex t == i then (t, idx) else findChildNode' i ts (idx + 1)

maxIndex :: [Int] -> Int
maxIndex [] = error "Cannot find the node with the maximum score"
maxIndex ns = head (elemIndices (maximum ns) ns)





{-


-- start with easy estimation
-- retrieve a score of an internal node for selection, hence, root does not needed
-- estimateNode :: PlayerIndex -> HistoryTree -> Visits -> GameTree -> Double
-- estimateNode i ht p gt = estimate i ht (getHashBoard gt) (getWins p gt) (getVisits gt) p
estimateNode :: GameTree -> Int
estimateNode gt = centroid (getOccupiedBoard gt)
    where
        getWins :: PlayerIndex -> GameTree -> Wins
        getWins i gt = getTuple gt !! i

        -- the selection strategy for "profit" of a node
        estimate :: PlayerIndex -> HistoryTree -> Hash -> Wins -> Visits -> Visits -> Double
        estimate i t x w v p = calculateUCT w v p + calculatePH i t x w v

        -- UCT formula
        calculateUCT :: Wins -> Visits -> Visits -> Double
        calculateUCT w v p = (fromIntegral w / fromIntegral v) + 0.5 * sqrt (log (fromIntegral p) / fromIntegral v)

        -- Progressive History formula
        calculatePH :: PlayerIndex -> HistoryTree -> Hash -> Wins -> Visits -> Double
        calculatePH i t x w v = case rbSearch x t of
                                    Just ws -> (fromIntegral (ws !! i) / fromIntegral (sum ws)) * (5 / fromIntegral (v - w + 1))
                                    Nothing -> 0 -- if this node is not played before





-- implement the list of movements and the resulting board states
flipListsH :: OccupiedBoard -> Hash -> [(Pos, [Pos])] -> [(OccupiedBoard, Hash, Pos, Pos)]
flipListsH _ _ [] = []
flipListsH b h (x:xs) = let (p, ps) = x
                        in  map (flipBoardStateH b h p) ps ++ flipListsH b h xs
    where
        -- exchange two pieces' states on the occupied board
        flipBoardStateH :: OccupiedBoard -> Hash -> Pos -> Pos -> (OccupiedBoard, Hash, Pos, Pos)
        flipBoardStateH b h f@(fx, fy) t@(tx, ty) = let newRow1 = replace fx (flip $ getElement b (fx, fy)) (b !! fy)
                                                        newBoard1 = replace fy newRow1 b
                                                        newRow2 = replace tx (flip $ getElement newBoard1 (tx, ty)) (newBoard1 !! ty)
                                                    in  (replace ty newRow2 newBoard1, hashChange f t h, f, t)
        flip :: Int -> Int
        flip 0 = 1
        flip _ = 0
-}
{-




updateTuple :: PlayerIndex -> Tuple -> Tuple
updateTuple i ws = let w = ws !! i
                   in  replace i (w+1) ws



-- bedies, the history tree is also needed to be update
updateHistoryTree :: PlayerIndex -> HistoryTree -> Trace -> HistoryTree
updateHistoryTree _ RBLeaf xs = if null xs then RBLeaf else error "The trace is not correct"
updateHistoryTree i ht xs = let (h, ys) = pop xs
                            in  updateHistoryTree i (htEdit h i ht) ys
    where
        -- edit a node with certain hashed index in the tree
        htEdit :: Hash -> PlayerIndex -> RBTree -> RBTree
        htEdit h i RBLeaf = RBLeaf
        htEdit h i (RBNode c v t1 x t2)
            | h > x = RBNode c v t1 x (htEdit h i t2)
            | h < x = RBNode c v (htEdit h i t1) x t2
            | otherwise = let newTuple = updateTuple i v
                        in RBNode c newTuple t1 x t2
-}