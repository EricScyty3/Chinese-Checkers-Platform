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
import Data.Maybe


type Wins = Int
type PlayerIndex = Int
type BoardIndex = Int
-- the index of a board state is not only used to determine a board but also useful when determining a node since the index is unique
type Transform = (BoardPos, BoardPos)
-- the conversion between two boards it done through knowing the changed positions

-- the game tree contains two main types: leaf and internal node with at least one child
-- normally, each node should hold the overall board of the game and a tuple of size N = players number representing the scroe for each player
-- but this could be inefficient if the board is large, hence, there nodes other than root are representing current board with "transformation"
-- the piece change delivered from the parent state is cause of the change of board, therefore, it is sufficient to only storing position changes as the board
data GameTree = GRoot BoardIndex [GameTree] |
                GLeaf BoardIndex Transform [Wins]  |
                GNode BoardIndex Transform [Wins] [GameTree]
                deriving (Eq, Show)
-- the options of playout policy
data PlayoutPolicy = MoveEvaluator | BoardEvaluator | ShallowSearch deriving (Eq, Show, Read)
-- in addition to the game tree itself, a history trace of how a move performs in previous game is also maintained
-- which could be useful at the early stage of movement selection when no much moves are experienced 
type HistoryTrace = RBTree [Wins]
-- the game status
type GameTreeStatus = (PlayerIndex, -- current player of the turn
                       BoardIndex, -- the unique id for indicating a board (the node)
                       Board, -- the board state delivered from the parent
                       [[Pos]], -- the list of positions of the internal board for each player
                       Int, -- the total players
                       HistoryTrace, -- the history performance of each move played in the past
                       (Double, Double), -- the arguments for selection strategy
                       PlayoutPolicy) -- the arguments for playout strategy

--Encapsulation----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- since the game state is warpped in the state monad, several toolkits are applied to access and modify and game state 
-- get the current player's index
getPlayerIdx :: State GameTreeStatus PlayerIndex
getPlayerIdx = do (pi, _, _, _, _, _, _, _) <- get; return pi
-- based on the player's index, return the corresponding colour of the pieces
getPlayerColour :: State GameTreeStatus Colour
getPlayerColour = do pi <- getPlayerIdx; playerColour pi <$> getPlayerNum
-- get the board index accumulated so far
getBoardIdx :: State GameTreeStatus BoardIndex
getBoardIdx = do (_, bi, _, _, _, _, _, _) <- get; return bi
-- return the board from the state
getBoard :: State GameTreeStatus Board
getBoard = do (_, _, b, _, _, _, _, _) <- get; return b
-- return the board hash for current player
getCurrentInternalBoard :: State GameTreeStatus [Pos]
getCurrentInternalBoard = do ps <- getInternalBoard
                             pi <- getPlayerIdx
                             return (ps !! pi)
-- return the positions list
getInternalBoard :: State GameTreeStatus [[Pos]]
getInternalBoard = do (_, _, _, ps, _, _, _, _) <- get; return ps
-- return the player's amount from the state
getPlayerNum :: State GameTreeStatus Int
getPlayerNum = do (_, _, _, _, pn, _, _, _) <- get; return pn
-- return the history of moves' information from the state
getHistoryTrace :: State GameTreeStatus HistoryTrace
getHistoryTrace = do (_, _, _, _, _, ht, _, _) <- get; return ht
-- return the two argurmant of selection strategy: UCT and PH
getUCTCons :: State GameTreeStatus Double
getUCTCons = do (_, _, _, _, _, _, (uct, _), _) <- get; return uct
getPHCons :: State GameTreeStatus Double
getPHCons = do (_, _, _, _, _, _, (_, ph), _) <- get; return ph
getPlayoutPolicy :: State GameTreeStatus PlayoutPolicy
getPlayoutPolicy = do (_, _, _, _, _, _, _, pp) <- get; return pp

-- update the player index based on the turn order
updatePlayerIdx :: State GameTreeStatus ()
updatePlayerIdx = do (pi, bi, b, ps, pn, ht, cons, pp) <- get; put (turnBase pn pi, bi, b, ps, pn, ht, cons, pp)
setPlayerIdx :: PlayerIndex -> State GameTreeStatus ()
setPlayerIdx pi = do (_, bi, b, ps, pn, ht, cons, pp) <- get; put (pi, bi, b, ps, pn, ht, cons, pp)
-- change the player turns based on index from 0 to the number - 1
turnBase :: Int -> PlayerIndex -> PlayerIndex
turnBase players idx = if idx == players - 1 then 0 else idx + 1
-- increment the board index by 1
updateBoardIdx :: State GameTreeStatus ()
updateBoardIdx = do (pi, bi, b, ps, pn, ht, cons, pp) <- get; put (pi, bi+1, b, ps, pn, ht, cons, pp)
-- set the current external board
setBoard :: Board -> State GameTreeStatus ()
setBoard b = do (pi, bi, _, ps, pn, ht, cons, pp) <- get; put (pi, bi, b, ps, pn, ht, cons, pp)
-- set the position list of the internal board for each player
setInternalBoard :: [[Pos]] -> State GameTreeStatus ()
setInternalBoard ps = do (pi, bi, b, _, pn, ht, cons, pp) <- get; put (pi, bi, b, ps, pn, ht, cons, pp)
-- given a position change, modify the corresponding board hash to generate a new one
modifyCurrentInternalBoard :: Transform -> State GameTreeStatus [Pos]
modifyCurrentInternalBoard (from, to) = do ps <- getCurrentInternalBoard
                                           colour <- getPlayerColour
                                           let pf = projection colour (getPos from) 
                                               pt = projection colour (getPos to)
                                               newps = pf `par` pt `pseq` flipBoard ps (pf, pt)
                                           return newps
-- update the list of internal board hash values
updateCurrentInternalBoard :: [Pos] -> State GameTreeStatus ()
updateCurrentInternalBoard ps = do psL <- getInternalBoard
                                   pi <- getPlayerIdx
                                   let npsL = replace pi ps psL
                                   setInternalBoard npsL
-- set the current history trace used for progressive history
setHistoryTrace :: HistoryTrace -> State GameTreeStatus ()
setHistoryTrace ht = do (pi, bi, b, ps, pn, _, cons, pp) <- get; put (pi, bi, b, ps, pn, ht, cons, pp)

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
getBoardIndex (GRoot idx _) = idx
getBoardIndex (GNode idx _ _ _) = idx
getBoardIndex (GLeaf idx _ _) = idx
-- return the child nodes of a node
getChildren :: GameTree -> [GameTree]
getChildren (GRoot _ ts) = ts
getChildren (GNode _ _ _ ts) = ts
getChildren GLeaf {} = []

-- get how many wins each player is reached in a list 
getWins :: GameTree -> [Wins]
getWins (GNode _ _ ws _) = ws
getWins (GLeaf _ _ ws) = ws
-- root has not wins since it has no parent but the wins can be retrieved from its children
getWins (GRoot _ ts) = if null ts then []
                       else let ws = map getWins ts
                            in  sumWins ws
    where
        sumWins :: [[Wins]] -> [Wins]
        sumWins [] = []
        sumWins [w] = w
        sumWins (w:ws) = zipWith (+) w (sumWins ws)
-- return the total times of a node being visited
getVisits :: GameTree -> Int
getVisits gt = sum (getWins gt) -- the sum of the win counts equals to the total visits of that node
-- return the piece/psotion change that between two boards
getTransform :: GameTree -> Transform
getTransform (GLeaf _ ft _) = ft
getTransform (GNode _ ft _ _) = ft
getTransform GRoot {} = (U(-1, -1), U(-1, -1))

--Game Tree Handling----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-
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
                                                       else evalState (repaintBoard $ getTransform tree) (0, 0, board, [], 0, RBLeaf, (0, 0), BoardEvaluator))
                                       in  if getBoardIndex tree == index then [(tree, newBoard)] -- return the found node and corresponding board
                                           else if null $ getChildren tree then [] -- return empty list if reaching leaf
                                                else concatMap (searchNode' index newBoard) (getChildren tree) -- expand the search to each child node
-}
-- given pieces change, generate a new board 
repaintBoard :: Transform-> State GameTreeStatus Board
repaintBoard (start, end) = do board <- getBoard; return (repaintPath board start end) -- recolour the start and end positions

-- given a certain piece's colour, return a list of avaliable movements (transforms)
colouredMovesList :: PlayerIndex -> State GameTreeStatus [Transform]
colouredMovesList idx = do board <- getBoard
                           psL <- getInternalBoard 
                           pn <- getPlayerNum
                           let ps = psL !! idx
                               colour = playerColour idx pn
                               bs = ps `par` colour `pseq` map (appendColour colour . reversion colour) ps -- invert the internal positions to external ones
                               ds = evalState (do mapM destinationList bs) board -- the new positions resulting from moving the above pieces
                           return (pairArrange bs ds) -- zip the resulting movements with the current pieces
pairArrange :: [BoardPos] -> [[BoardPos]] -> [Transform]
pairArrange _ [] = []
pairArrange [] _ = []
pairArrange (b:bs) (d:ds) = zip (repeat b) d ++ pairArrange bs ds

-- return the current player's colour based on the number of players
playerColour :: PlayerIndex -> Int -> Colour
playerColour idx number = playerColourList number !! idx

-- edit a certain wins for a node by incrementing 1 for a win
editNodeValue :: PlayerIndex -> GameTree -> GameTree
editNodeValue _ (GRoot i ts) = GRoot i ts
editNodeValue pi (GLeaf i ft ws) = let new = replace pi ((ws!!pi)+1) ws
                                   in  GLeaf i ft new
editNodeValue pi (GNode i ft ws ts) = let new = replace pi ((ws!!pi)+1) ws
                                      in  GNode i ft new ts

-- change the child nodes for a node
editNodeChildren :: [GameTree] -> GameTree -> GameTree
editNodeChildren [] t = t -- no change is made if the children are empty
editNodeChildren ts (GRoot i _) = GRoot i ts
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
estimateNodeUCT :: Int -> Int -> State GameTreeStatus Double
estimateNodeUCT parentVisits visits = if visits == 0 then return 0
                                      else do constant <- getUCTCons
                                              return $ constant * sqrt (log (fromIntegral parentVisits) / fromIntegral visits)
-- consider additionally the similar move not only from parent nodes but the history
-- that is, the average score of a certain move being performed
-- tree-only PH: only consider the moves selected in the selection stages rather than both selection and playout stages
estimateNodePH :: Int -> Int -> [Wins] -> State GameTreeStatus Double
estimateNodePH bhash visits wins = do pi <- getPlayerIdx
                                      ht <- getHistoryTrace
                                      constant <- getPHCons
                                      case rbSearch bhash ht of -- and find if similar move has been made in the past
                                        Nothing -> return 0
                                        Just wins -> return $ fromIntegral (wins !! pi) / fromIntegral (sum wins)
                                                            * constant / fromIntegral (visits - (wins !! pi) + 1)
                                                -- if it does, then return the calculated result
-- when estimating a node, skip if the entered node is root, otherwise, apply the formulas above
estimateNode :: Int -> GameTree -> State GameTreeStatus Double
estimateNode pv node = if isRoot node then return 0
                       else do pi <- getPlayerIdx
                               colour <- getPlayerColour
                               eboard <- getBoard
                               ps <- modifyCurrentInternalBoard (getTransform node)
                               let mean = averageScore pi node
                                   visits = getVisits node
                                   wins = getWins node
                               uct <- estimateNodeUCT pv visits
                               ph  <- estimateNodePH (hash ps) visits wins
                               return $ uct `par` ph `pseq` mean + uct + ph

