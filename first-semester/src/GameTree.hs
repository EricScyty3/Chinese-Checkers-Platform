module GameTree where
-- the data structure that is used in game theory containing the nodes representing the game states
import Data.List
import Zobrist
import Board
import Control.Monad.State
import Control.Monad.ST
import Data.STRef
import RBTree
import Control.Parallel


type Wins = Int
type PlayerIndex = Int
type BoardIndex = Int 
-- the index of a board state is not only used to determine a board but also useful when determining a node since the index is unique
type Transform = (BoardPos, BoardPos) 
-- the conversion between two boards it done through knowing the changed positions

-- the game tree contains two main types: leaf and internal node with at least one child
-- the root node is represented individually here to contain the whole board while others don't
-- normally, each node should hold the overall board of the game and a tuple of size N = players number representing the scroe for each player
-- but this could be inefficient if the board is large, hence, there nodes other than root are representing current board with "transformation"
-- the piece change delivered from the parent state is cause of the change of board, therefore, it is sufficient to only storing position changes as the board
data GameTree = GRoot BoardIndex Board [Wins] [GameTree] |
                GLeaf BoardIndex Transform [Wins]  |
                GNode BoardIndex Transform [Wins] [GameTree]
                deriving (Eq, Show)

-- in addition to the game tree itself, a history trace of how a move performs in previous game is also maintained
-- which could be useful at the early stage of movement selection when no much moves are experienced 
type HistoryTrace = RBTree [Wins]

-- the game status
type GameTreeStatus = (PlayerIndex, -- current player of the turn
                       BoardIndex, -- the unique id for indicating a board
                       Board, -- the board state delivered from the parent
                       Int, -- the total players
                       HistoryTrace, -- the history performance of each move played in the past
                       (Double, Double)) -- the arguments for selection strategy

--Encapsulation----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- since the game state is warpped in the state monad, several toolkits are applied to access and modify and game state 
-- get the current player's index
getPlayerIdx :: State GameTreeStatus PlayerIndex
getPlayerIdx = do (pi, _, _, _, _, _) <- get; return pi
-- based on the player's index, return the corresponding colour of the pieces
getPlayerColour :: State GameTreeStatus Colour
getPlayerColour = do pi <- getPlayerIdx; playerColour pi <$> getPlayerNum
-- get the board index accumulated so far
getBoardIdx :: State GameTreeStatus BoardIndex
getBoardIdx = do (_, bi, _, _, _, _) <- get; return bi
-- return the board from the state
getBoard :: State GameTreeStatus Board
getBoard = do (_, _, b, _, _, _) <- get; return b
-- return the player's amount from the state
getPlayerNum :: State GameTreeStatus Int
getPlayerNum = do (_, _, _, pn, _, _) <- get; return pn
-- return the history of moves' information from the state
getHistoryTrace :: State GameTreeStatus HistoryTrace
getHistoryTrace = do (_, _, _, _, ht, _) <- get; return ht
-- return the two argurmant of selection strategy: UCT and PH
getUCTCons :: State GameTreeStatus Double
getUCTCons = do (_, _, _, _, _, (uct, _)) <- get; return uct
getPHCons :: State GameTreeStatus Double
getPHCons = do (_, _, _, _, _, (_, ph)) <- get; return ph

-- update the player index based on the turn order
updatePlayerIdx :: State GameTreeStatus ()
updatePlayerIdx = do (pi, bi, b, pn, ht, cons) <- get; put (turnBase pn pi, bi, b, pn, ht, cons)
-- change the player turns based on index from 0 to the number - 1
turnBase :: Int -> PlayerIndex -> PlayerIndex
turnBase players idx = if idx == players - 1 then 0 else idx + 1
-- increment the board index by 1
updateBoardIdx :: State GameTreeStatus ()
updateBoardIdx = do (pi, bi, b, pn, ht, cons) <- get; put (pi, bi+1, b, pn, ht, cons)
-- update the current board that it transformed by parent node
updateBoard :: Board -> State GameTreeStatus ()
updateBoard b = do (pi, bi, _, pn, ht, cons) <- get; put (pi, bi, b, pn, ht, cons)
-- add or eidt the moves performed in previous games
updateHistoryTrace :: HistoryTrace -> State GameTreeStatus ()
updateHistoryTrace ht = do (pi, bi, b, pn, _, cons) <- get; put (pi, bi, b, pn, ht, cons)

-- determine the game tree node type: root, node, and leaf
getNodeType :: GameTree -> String
getNodeType GRoot {} = "Root"
getNodeType GNode {} = "Node"
getNodeType GLeaf {} = "Leaf"
-- check whether a node is a root
isRoot :: GameTree -> Bool
isRoot GRoot {} = True
isRoot _ = False
-- return the board index of a node
getBoardIndex :: GameTree -> BoardIndex
getBoardIndex (GRoot idx _ _ _) = idx
getBoardIndex (GNode idx _ _ _) = idx
getBoardIndex (GLeaf idx _ _) = idx
-- return the board state stored in the root node
getRootBoard :: GameTree -> Board
getRootBoard (GRoot _ b _ _) = b
getRootBoard _ = []
-- return the child nodes of a node
getChildren :: GameTree -> [GameTree]
getChildren (GRoot _ _ _ ts) = ts
getChildren (GNode _ _ _ ts) = ts
getChildren GLeaf {} = []
-- get how many wins each player is reached in a list 
getWins :: GameTree -> [Wins]
getWins (GRoot _ _ ws _) = ws
getWins (GNode _ _ ws _) = ws
getWins (GLeaf _ _ ws) = ws
-- return the total times of a node being visited
getVisits :: GameTree -> Int
getVisits gt = sum (getWins gt) -- the sum of the win counts equals to the total visits of that node
-- return the piece/psotion change that between two boards
getTransform :: GameTree -> Transform
getTransform (GLeaf _ ft _) = ft
getTransform (GNode _ ft _ _) = ft
getTransform GRoot {} = (U(-1, -1), U(-1, -1))

--Game Tree Handling----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- search and print a node' information in a given game tree based on entered index
showNode :: GameTree -> BoardIndex -> IO()
showNode gt idx = let (foundNode, board) = searchNode idx gt
                  in  do printEoard board
                         printGameTree foundNode
-- list the feature of a game tree node
printGameTree :: GameTree -> IO ()
printGameTree gt = do putStrLn ("Index: " ++ show (getBoardIndex gt))
                      putStrLn ("Tyep: " ++ getNodeType gt)
                      putStrLn ("Win List: " ++ show(getWins gt))
                      putStrLn ("Children: " ++ show(length $ getChildren gt))
-- search for a node based on the unique board indexin the game tree
searchNode :: BoardIndex -> GameTree -> (GameTree, Board)
searchNode i t = let ls = searchNode' i (getRootBoard t) t
                 in  if null ls then error "Not exist" else head ls
    where
        -- start from root and keep updateing the board while searching for the specific node with certain index
        searchNode' :: BoardIndex -> Board -> GameTree -> [(GameTree, Board)]
        searchNode' index board tree = let newBoard = (if isRoot tree then board
                                                       else evalState (repaintBoard $ getTransform tree) (0, 0, board, 0, RBLeaf, (0, 0)))
                                       in  if getBoardIndex tree == index then [(tree, newBoard)] -- return the found node and corresponding board
                                           else if null $ getChildren tree then [] -- return empty list if reaching leaf
                                                else concatMap (searchNode' index newBoard) (getChildren tree) -- expand the search to each child node
-- given pieces change, generate a new board 
repaintBoard :: Transform-> State GameTreeStatus Board
repaintBoard (start, end) = do board <- getBoard
                               let colour = Board.getColour start
                                   eboard = changeBoardElement erase start board -- erase the start position
                               return (eboard `par` colour `pseq` changeBoardElement (safeRepaint colour) end eboard) -- recolour the end position

-- given a certain piece's colour, return a list of avaliable movements (transforms)
colouredMovesList :: Colour -> State GameTreeStatus [Transform]
colouredMovesList colour = do board <- getBoard
                              let bs = findPiecesWithColour colour board -- all pieces of certain colour
                                  ds = evalState (do mapM destinationList bs) board -- the new positions resulting from moving the above pieces
                              return (pairArrange bs ds) -- zip the resulting movements with the current pieces
    where
        pairArrange :: [BoardPos] -> [[BoardPos]] -> [Transform]
        pairArrange _ [] = []
        pairArrange [] _ = []
        pairArrange (b:bs) (d:ds) = zip (repeat b) d ++ pairArrange bs ds

-- return the current player's colour based on the number of players
playerColour :: PlayerIndex -> Int -> Colour
playerColour idx number
    | number == 2 = twoPlayersSet !! idx
    | number == 3 = threePlayersSet !! idx
    | number == 4 = fourPlayersSet !! idx
    | otherwise = sixPlayersSet !! idx

-- edit a certain wins for a node by incrementing 1 for a win
editNodeValue :: PlayerIndex -> GameTree -> GameTree
editNodeValue pi (GRoot i b ws ts) = let new = replace pi ((ws!!pi)+1) ws
                                     in  GRoot i b new ts
editNodeValue pi (GLeaf i ft ws) = let new = replace pi ((ws!!pi)+1) ws
                                   in  GLeaf i ft new
editNodeValue pi (GNode i ft ws ts) = let new = replace pi ((ws!!pi)+1) ws
                                      in  GNode i ft new ts

-- change the child nodes for a node
editNodeChildren :: [GameTree] -> GameTree -> GameTree
editNodeChildren [] t = t -- no change is made if the children are empty
editNodeChildren ts (GRoot i b ws _) = GRoot i b ws ts
editNodeChildren ts (GNode i ft ws _) = GNode i ft ws ts
editNodeChildren ts (GLeaf i ft ws) = GNode i ft ws ts -- a leaf becomes internal node when having children nodes 

--Node Evaluation-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- determine a node's profits based on different measurements

-- the average of how well a move/node is performing based on dividing the total wins with total visits
averageScore :: PlayerIndex -> GameTree -> Double
averageScore i t = if getVisits t == 0 then 0 -- if not visit is made then just return 0
                   else fromIntegral (getWins t !! i) / fromIntegral (getVisits t)

-- determine a node's profits accroding to the UCT formula
-- consider the total visits of the parent node as well as the child node
estimateNodeUCT :: Int -> GameTree -> State GameTreeStatus Double
estimateNodeUCT parentVisits node = if getVisits node == 0 then return 0
                                    else do constant <- getUCTCons
                                            return $ constant * 
                                                     sqrt (log (fromIntegral parentVisits) / fromIntegral (getVisits node))

-- consider additionally the similar move not only from parent nodes but the history
-- that is, the average score of a certain move being performed
-- tree-only PH: only consider the moves selected in the selection stages rather than both selection and playout stages
estimateNodePH :: GameTree -> State GameTreeStatus Double
estimateNodePH node = let (from, to) = getTransform node -- first get the transformed positions for searching the history
                      in  case Board.getColour from of
                            Nothing -> error "Estimate: cannot determine current player's colour"
                            Just colour -> do pi <- getPlayerIdx
                                              ht <- getHistoryTrace
                                              constant <- getPHCons
                                              let pf = projection colour (getPos from) -- project the positions to occupied board
                                                  pt = projection colour (getPos to)
                                              case pf `par` pt `pseq` rbSearch (hash [pf, pt]) ht of -- and find if similar move has been made in the past
                                                    Nothing -> return 0
                                                    Just wins -> return $ fromIntegral (wins !! pi) / fromIntegral (sum wins)
                                                                          * constant / fromIntegral (getVisits node - (getWins node !! pi) + 1)
                                                                          -- if it does, then return the calculated result

-- when estimating a node, skip if the entered node is root, otherwise, apply the formulas above
estimateNode :: Int -> GameTree -> State GameTreeStatus Double
estimateNode pv node = if isRoot node then return 0
                       else do   
                          pi <- getPlayerIdx
                          let mean = averageScore pi node
                          uct <- estimateNodeUCT pv node
                          ph  <- estimateNodePH node
                          return $ uct `par` ph `pseq` mean + uct + ph

-- since the history trace should be updated as well every time a selection is made
editHT :: GameTree -> PlayerIndex -> Int -> HistoryTrace -> HistoryTrace
editHT node pi pn ht = if isRoot node then ht -- the root means no move is made, hence, skip
                       else let (from, to) = getTransform node
                            in  case Board.getColour from of
                                    Nothing -> error "Edit: cannot determine current player's colour"
                                    Just colour -> let pf = projection colour (getPos from)
                                                       pt = projection colour (getPos to)
                                                       h = hash [pf, pt] -- generate the hash key for locating the move in the history 
                                                   in  case pf `par` pt `pseq` rbSearch h ht of
                                                            Nothing -> rbInsert h initWins ht -- if not then add one
                                                            Just ws -> rbInsert h (replace pi ((ws!!pi)+1) ws) ht -- otherwise, update the wins
    where
        initWins = replace pi (initial !! pi + 1) initial
        initial  = replicate pn 0