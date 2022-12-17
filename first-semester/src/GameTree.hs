module GameTree where

import Data.List
import Zobrist
import Board
import Control.Monad.State
import Control.Monad.ST
import Data.STRef
import RBTree ( RBTree(..), Key, rbInsert, rbSearch )
import Control.Parallel

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
                GLeaf BoardIndex Transform [Wins]  |
                GNode BoardIndex Transform [Wins] [GameTree]
                deriving (Eq, Show)

type HistoryTrace = RBTree [Wins]
type GameTreeStatus = (PlayerIndex, BoardIndex, Board, Int, HistoryTrace, (Double, Double))

getPlayerIdx :: State GameTreeStatus PlayerIndex
getPlayerIdx = do (pi, _, _, _, _, _) <- get
                  return pi

getPlayerColour :: State GameTreeStatus Colour
getPlayerColour = do pi <- getPlayerIdx
                     currentPlayerColour pi <$> getPlayerNum

getBoardIdx :: State GameTreeStatus BoardIndex
getBoardIdx = do (_, bi, _, _, _, _) <- get
                 return bi
getBoard :: State GameTreeStatus Board
getBoard = do (_, _, b, _, _, _) <- get
              return b
getPlayerNum :: State GameTreeStatus Int
getPlayerNum = do (_, _, _, pn, _, _) <- get
                  return pn
getHistoryTrace :: State GameTreeStatus HistoryTrace
getHistoryTrace = do (_, _, _, _, ht, _) <- get
                     return ht
getUCTCons :: State GameTreeStatus Double
getUCTCons = do (_, _, _, _, _, (uct, _)) <- get
                return uct
getPHCons :: State GameTreeStatus Double
getPHCons = do (_, _, _, _, _, (_, ph)) <- get
               return ph

updatePlayerIdx :: State GameTreeStatus ()
updatePlayerIdx = do (pi, bi, b, pn, ht, cons) <- get
                     put (turnBase pn pi, bi, b, pn, ht, cons)
-- update the player turns based on index from 0 to the number - 1
turnBase :: Int -> PlayerIndex -> PlayerIndex
turnBase players idx = if idx == players - 1 then 0 else idx + 1

updateBoardIdx :: State GameTreeStatus ()
updateBoardIdx = do (pi, bi, b, pn, ht, cons) <- get
                    put (pi, bi+1, b, pn, ht, cons)

updateBoard :: Board -> State GameTreeStatus ()
updateBoard b = do (pi, bi, _, pn, ht, cons) <- get
                   put (pi, bi, b, pn, ht, cons)

updateHistoryTrace :: HistoryTrace -> State GameTreeStatus ()
updateHistoryTrace ht = do (pi, bi, b, pn, _, cons) <- get
                           put (pi, bi, b, pn, ht, cons)



-- contains the layer's player index for choosing the next move, board index accumulated so far, and the parent board state, 
-- as well as the total players amount, gaming history, and a pair of arguments for node evaluation 

printGameTree :: GameTree -> IO ()
printGameTree gt = do putStrLn ("Index: " ++ show (getBoardIndex gt))
                      putStrLn ("Tyep: " ++ getNodeType gt)
                      putStrLn ("Win List: " ++ show(getWins gt))
                      putStrLn ("Children: " ++ show(length $ getChildren gt))

searchNode :: BoardIndex -> GameTree -> (GameTree, Board)
searchNode i t = let ls = searchNode' i (getRootBoard t) t
                 in  if null ls then error "Not exist" else head ls
    where
        -- start from root 
        searchNode' :: BoardIndex -> Board -> GameTree -> [(GameTree, Board)]
        searchNode' index board tree = let newBoard = (if isRoot tree then board
                                                       else evalState (repaintBoard $ getTransform tree) (0, 0, board, 0, RBLeaf, (0, 0)))
                                       in  if getBoardIndex tree == index then [(tree, newBoard)]
                                           else if null $ getChildren tree then []
                                                else concatMap (searchNode' index newBoard) (getChildren tree)

-- given two pieces, exchange their colours
repaintBoard :: Transform-> State GameTreeStatus Board
repaintBoard (start, end) = do board <- getBoard
                               let colour = getColour start
                                   n1 = changeBoardElement erase start board
                               return (n1 `par` colour `pseq` changeBoardElement (safeRepaint colour) end n1)

getNodeType :: GameTree -> String
getNodeType GRoot {} = "Root"
getNodeType GNode {} = "Node"
getNodeType GLeaf {} = "Leaf"

isRoot :: GameTree -> Bool
isRoot GRoot {} = True
isRoot _ = False

getBoardIndex :: GameTree -> BoardIndex
getBoardIndex (GRoot idx _ _ _) = idx
getBoardIndex (GNode idx _ _ _) = idx
getBoardIndex (GLeaf idx _ _) = idx

getRootBoard :: GameTree -> Board
getRootBoard (GRoot _ b _ _) = b
getRootBoard _ = []

getChildren :: GameTree -> [GameTree]
getChildren (GRoot _ _ _ ts) = ts
getChildren (GNode _ _ _ ts) = ts
getChildren GLeaf {} = []

getWins :: GameTree -> [Wins]
getWins (GRoot _ _ ws _) = ws
getWins (GNode _ _ ws _) = ws
getWins (GLeaf _ _ ws) = ws

getVisits :: GameTree -> Int
getVisits gt = sum (getWins gt) -- the sum of the win counts equals to the total visits of that node

-- getPlayers :: GameTree -> Int
-- getPlayers gt = length (getWins gt) -- the length of win list equals to the total players number

getTransform :: GameTree -> Transform
getTransform (GLeaf _ ft _) = ft
getTransform (GNode _ ft _ _) = ft
getTransform GRoot {} = (U(-1, -1), U(-1, -1))

-- transform the display board into occupied board based on piece's colour 
projectCOB :: Colour -> State GameTreeStatus OccupiedBoard
projectCOB colour = do board <- getBoard
                       let ps = findPiecesWithColour colour board
                           cps = map (projection colour . getPos) ps
                       return (runST $ do n <- newSTRef empty
                                          modifySTRef n (fillBoard cps)
                                          readSTRef n)
    where
        fillBoard :: [Pos] -> OccupiedBoard -> OccupiedBoard
        fillBoard ps b = foldl (\ b p -> replace2 p 1 b) b ps

-- provide the available pieces and their movement pairs
-- work as board expansion
colouredMovesList :: Colour -> State GameTreeStatus [Transform]
colouredMovesList colour = do board <- getBoard
                              let bs = findPiecesWithColour colour board
                                  ds = evalState (do mapM destinationList bs) board
                              return (pairArrange bs ds)
    where
        pairArrange :: [BoardType] -> [[BoardType]] -> [(BoardType, BoardType)]
        pairArrange _ [] = []
        pairArrange [] _ = []
        pairArrange (b:bs) (d:ds) = let rl = replicate (length d) b
                                    in  zip rl d ++ pairArrange bs ds

-- return the current player's colour based on the number of players
currentPlayerColour :: PlayerIndex -> Int -> Colour
currentPlayerColour idx number
    | number == 2 = twoPlayersSet !! idx
    | number == 3 = threePlayersSet !! idx
    | number == 4 = fourPlayersSet !! idx
    | otherwise = sixPlayersSet !! idx

-- edit a certain win for a node
editNodeValue :: PlayerIndex -> GameTree -> GameTree
editNodeValue pi (GRoot i b ws ts) = let new = replace pi ((ws!!pi)+1) ws
                                     in  GRoot i b new ts
editNodeValue pi (GLeaf i ft ws) = let new = replace pi ((ws!!pi)+1) ws
                                   in  GLeaf i ft new
editNodeValue pi (GNode i ft ws ts) = let new = replace pi ((ws!!pi)+1) ws
                                      in  GNode i ft new ts
-- change the children node list for a node
editNodeChildren :: [GameTree] -> GameTree -> GameTree
editNodeChildren [] t = t
editNodeChildren ts (GRoot i b ws _) = GRoot i b ws ts
editNodeChildren ts (GNode i ft ws _) = GNode i ft ws ts
editNodeChildren ts (GLeaf i ft ws) = GNode i ft ws ts -- a leaf becomes internal node when having children nodes 
-- return the maximum value's index
maxIndex :: Ord a => [a] -> Int
maxIndex [] = error "Cannot find the node with the maximum score"
maxIndex ns = head (elemIndices (maximum ns) ns)

-- determine a node's profits simply based on how well it wins
averageScore :: PlayerIndex -> GameTree -> Double
averageScore i t = if getVisits t == 0 then 0
                   else fromIntegral (getWins t !! i) / fromIntegral (getVisits t)

-- determine a node's profits accroding to the UCT formula
-- consider the total visits of the parent node as well as the child node additionally
estimateNodeUCT :: Int -> GameTree -> State GameTreeStatus Double
estimateNodeUCT parentVisits node = if getVisits node == 0 then return 0
                                    else do constant <- getUCTCons
                                            return $ constant * sqrt (log (fromIntegral parentVisits) / fromIntegral (getVisits node))
    -- where
    --     constant :: Double
    --     constant = 0.5
-- additionally consider the similar move not only from parent nodes but the history trace
-- the average score of a certain move being performed
-- Progressive History 
editHT :: GameTree -> PlayerIndex -> Int -> HistoryTrace -> HistoryTrace
editHT node pi pn ht = if isRoot node then ht
                       else let (from, to) = getTransform node
                            in  case getColour from of
                                    Nothing -> error "Edit: cannot determine current player's colour"
                                    Just colour -> let pf = projection colour (getPos from)
                                                       pt = projection colour (getPos to)
                                                       h = hash [pf, pt]
                                                   in  case pf `par` pt `pseq` rbSearch h ht of
                                                            Nothing -> rbInsert h initWins ht
                                                            Just ws -> rbInsert h (replace pi ((ws!!pi)+1) ws) ht
    where
        initWins = replace pi (initial !! pi + 1) initial
        initial = replicate pn 0

-- tree-only PH
-- estimateNodePH :: Double ->PlayerIndex -> HistoryTrace -> GameTree -> Double
estimateNodePH :: GameTree -> State GameTreeStatus Double
estimateNodePH node = if isRoot node then return 0
                      else let (from, to) = getTransform node
                           in  case getColour from of
                                Nothing -> error "Estimate: cannot determine current player's colour"
                                Just colour -> do pi <- getPlayerIdx
                                                  ht <- getHistoryTrace
                                                  constant <- getPHCons
                                                  let pf = projection colour (getPos from)
                                                      pt = projection colour (getPos to)
                                                  case pf `par` pt `pseq` rbSearch (hash [pf, pt]) ht of
                                                    Nothing -> return 0
                                                    Just wins -> return $ fromIntegral (wins !! pi) / fromIntegral (sum wins)
                                                                          * constant / fromIntegral (getVisits node - (getWins node !! pi) + 1)
    -- where
    --     constant :: Double
    --     constant = 5

-- estimateNode :: (Double, Double) -> PlayerIndex -> Int -> HistoryTrace -> GameTree -> Double
estimateNode :: Int -> GameTree -> State GameTreeStatus Double
estimateNode pv node = do pi <- getPlayerIdx
                          let mean = averageScore pi node
                          uct <- estimateNodeUCT pv node
                          ph  <- estimateNodePH node
                          return $ uct `par` ph `pseq` mean + uct + ph
