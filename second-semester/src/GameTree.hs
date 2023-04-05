module GameTree where
-- the data structure that is used during the search, the game states are represented as nodes in the search tree

import Zobrist ( flipBoard, hash )
import Board
    ( appendColour,
      destinationList,
      playerColourList,
      projectMove,
      repaintPath,
      replace,
      reversion,
      Board,
      BoardPos(U),
      Colour,
      Pos,
      Transform, defaultMove )
import Control.Monad.State
    ( State, evalState, MonadState(put, get) )
import RBTree ( rbSearch, RBTree )
import Control.Parallel ( par, pseq )
import System.Random ( StdGen )

-- several re-naming for better understanding
-- a list of integers containing how many times all player can win at the end 
type Wins = [Int]
-- the index of a player is used to determine who is to perform a move based on the current board state
type PlayerIndex = Int
-- the index of a certain board state is used to determine the expansion of the search tree 
type BoardIndex = Int

-- the game tree contains two main types: leaf with not child and internal node with at least one child
-- each node should hold a tuple of size N = players number representing the score for each player
-- besides, nodes other than root contains a movement showing how its board state is transformed from its parent 
-- since the change delivered from the parent state is cause of the movement, therefore, it is sufficient to only storing position changes
data GameTree = GRoot BoardIndex [GameTree] |
                GLeaf BoardIndex Transform Wins  |
                GNode BoardIndex Transform Wins [GameTree]
                deriving (Eq, Show)

-- the options of playout evaluator, how the board is evaluated during the self-played simulation
data PlayoutEvaluator = Random | Move | Board | MParanoid | MBRS deriving (Eq, Show, Read)
-- the two last moves that could cause a cutoff for certain layer, and might be helpful for the next search (applied when the evaluator is minimax-based)
type KillerMoves = [Transform]
-- the parameter pair of the evaluator, the search depth, and the record of cutoff moves
type PlayoutArgument = (PlayoutEvaluator, Int, [KillerMoves])
-- in addition to the game tree, a history trace of how a move performs in previous game is also maintained
-- which could be useful at the early stage of the algorithm where no much moves are experienced and often compromise to random selection
type HistoryTrace = RBTree Wins

-- the game status, all the sufficient information needed to represent a game state
type GameTreeStatus = (-- the random number generator, which should be assigned by IO action first
                       StdGen,
                       -- the player of the current turn
                       PlayerIndex,
                       -- the maximum board id for indicating the total nodes
                       BoardIndex,
                       -- the board state inherited from the parent through certain Transform
                       Board,
                       -- the list of projected positions for each player
                       [[Pos]],
                       -- the total players, should be either 2, 3, 4, or 6
                       Int,
                       -- the history performance of each move played in the past
                       HistoryTrace,
                       -- the constant pair for the selection policy
                       (Double, Double),
                       -- the arguments for the playout strategy
                       PlayoutArgument)

--Encapsulation----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- since the game state is wrapped in the state monad, several toolkits are applied to access and modify and game state 

-- get the current player's index
getPlayerIdx :: State GameTreeStatus PlayerIndex
getPlayerIdx = do (_, pi, _, _, _, _, _, _, _) <- get; return pi

-- based on the current player's index, return the corresponding colour
getPlayerColour :: State GameTreeStatus Colour
getPlayerColour = do pi <- getPlayerIdx; playerColour pi <$> getPlayerNum

-- get the board index accumulate so far
getBoardIdx :: State GameTreeStatus BoardIndex
getBoardIdx = do (_, _, bi, _, _, _, _, _, _) <- get; return bi

-- return the board from the state
getBoard :: State GameTreeStatus Board
getBoard = do (_, _, _, eboard, _, _, _, _, _) <- get; return eboard

-- return the board hash for current player
getCurrentInternalBoard :: State GameTreeStatus [Pos]
getCurrentInternalBoard = do iboards <- getInternalBoards
                             pi <- getPlayerIdx
                             return $ iboards `par` pi `pseq` (iboards !! pi)

-- return the positions list
getInternalBoards :: State GameTreeStatus [[Pos]]
getInternalBoards = do (_, _, _, _, iboards, _, _, _, _) <- get; return iboards

-- return the player's amount from the state
getPlayerNum :: State GameTreeStatus Int
getPlayerNum = do (_, _, _, _, _, pn, _, _, _) <- get; return pn

-- return the history of moves' information from the state
getHistoryTrace :: State GameTreeStatus HistoryTrace
getHistoryTrace = do (_, _, _, _, _, _, ht, _, _) <- get; return ht

-- return the two argument of selection strategy: UCT and PH
getUCTCons :: State GameTreeStatus Double
getUCTCons = do (_, _, _, _, _, _, _, (uct, _), _) <- get; return uct
getPHCons :: State GameTreeStatus Double
getPHCons = do (_, _, _, _, _, _, _, (_, ph), _) <- get; return ph

-- return the playout arguments: evaluator, depth and killer moves
getPlayoutArgument :: State GameTreeStatus PlayoutArgument
getPlayoutArgument = do (_, _, _, _, _, _, _, _, pa) <- get; return pa

-- update the player index based on the turn order
updatePlayerIdx :: State GameTreeStatus ()
updatePlayerIdx = do (gen, pi, bi, b, ps, pn, ht, cons, pa) <- get; put (gen, turnBase pn pi, bi, b, ps, pn, ht, cons, pa)

-- change the player turns based on index from 0 to the number - 1
turnBase :: Int -> PlayerIndex -> PlayerIndex
turnBase players idx = if idx == players - 1 then 0 else idx + 1

-- revert the turn to the last one
turnRevert :: Int -> PlayerIndex -> PlayerIndex
turnRevert players idx
  | idx == 0 = players - 1
  | otherwise = idx - 1

-- increment the board index by 1
updateBoardIdx :: State GameTreeStatus ()
updateBoardIdx = do (gen, pi, bi, b, ps, pn, ht, cons, pa) <- get; put (gen, pi, bi+1, b, ps, pn, ht, cons, pa)

-- set the current external board to a new one
setBoard :: Board -> State GameTreeStatus ()
setBoard b = do (gen, pi, bi, _, ps, pn, ht, cons, pa) <- get; put (gen, pi, bi, b, ps, pn, ht, cons, pa)

-- set a new position list of the internal board for all player
setInternalBoards :: [[Pos]] -> State GameTreeStatus ()
setInternalBoards ps = do (gen, pi, bi, b, _, pn, ht, cons, pa) <- get; put (gen, pi, bi, b, ps, pn, ht, cons, pa)

-- given a position change, modify the corresponding internal board
modifyCurrentInternalBoard :: Transform -> State GameTreeStatus [Pos]
modifyCurrentInternalBoard tf = do ps <- getCurrentInternalBoard
                                   colour <- getPlayerColour
                                   let ptf = projectMove colour tf
                                   return $ ps `par` ptf `pseq` flipBoard ps ptf

-- update the list of internal board with new updated entity
setCurrentInternalBoard :: [Pos] -> State GameTreeStatus ()
setCurrentInternalBoard ps = do iboards <- getInternalBoards
                                pi <- getPlayerIdx
                                let niboards = pi `par` iboards `pseq` replace pi ps iboards
                                setInternalBoards niboards

-- reset the current history trace used for progressive history
setHistoryTrace :: HistoryTrace -> State GameTreeStatus ()
setHistoryTrace ht = do (gen, pi, bi, b, ps, pn, _, cons, pa) <- get; put (gen, pi, bi, b, ps, pn, ht, cons, pa)

-- reset the playout argument with new one
setPlayoutArgument :: PlayoutArgument -> State GameTreeStatus ()
setPlayoutArgument pa = do (gen, pi, bi, b, ps, pn, ht, cons, _) <- get; put (gen, pi, bi, b, ps, pn, ht, cons, pa)

-- reset the killer moves
setPlayoutKillerMoves :: [[Transform]] -> State GameTreeStatus ()
setPlayoutKillerMoves killerMoves = do (eval, depth, _) <- getPlayoutArgument
                                       setPlayoutArgument (eval, depth, killerMoves)

-- return the random number generator
getRandGen :: State GameTreeStatus StdGen
getRandGen = do (gen, _, _, _, _, _, _, _, _) <- get; return gen

-- renew the random number generator
setRandGen :: StdGen -> State GameTreeStatus ()
setRandGen gen = do (_, pi, bi, b, ps, pn, ht, cons, pa) <- get
                    put (gen, pi, bi, b, ps, pn, ht, cons, pa)

--Tree Operators----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- the functions related to the search tree entities

-- check whether a node is a root
isRoot :: GameTree -> Bool
isRoot GRoot {} = True
isRoot _ = False

-- return the board index of a node
getBoardIndex :: GameTree -> BoardIndex
getBoardIndex (GRoot idx _) = idx
getBoardIndex (GNode idx _ _ _) = idx
getBoardIndex (GLeaf idx _ _) = idx

-- return the children of a node
getChildren :: GameTree -> [GameTree]
getChildren (GRoot _ ts) = ts
getChildren (GNode _ _ _ ts) = ts
getChildren GLeaf {} = []

-- get how many wins each player is reached in a list 
getWins :: GameTree -> Wins
getWins (GNode _ _ ws _) = ws
getWins (GLeaf _ _ ws) = ws

-- root has not wins can be retrieved from its children
getWins (GRoot _ ts) = if null ts then []
                       else let ws = map getWins ts
                            in  sumWins ws
    where
        sumWins :: [Wins] -> Wins
        sumWins [] = []
        sumWins [w] = w
        sumWins (w:ws) = zipWith (+) w (sumWins ws)

-- return the total times of a node being visited
getVisits :: GameTree -> Int
getVisits t = sum (getWins t) -- the sum of the wins equals to the total visits of that node

-- return the position change of the node from its parent's state
getTransform :: GameTree -> Transform
getTransform (GLeaf _ ft _) = ft
getTransform (GNode _ ft _ _) = ft
getTransform GRoot {} = defaultMove -- the root is not inherited from any node, so set a default value here

--Game Tree Handling----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- given pieces change, generate a new board 
repaintBoard :: Transform-> State GameTreeStatus Board
repaintBoard tf = do board <- getBoard;
                     return (repaintPath board tf)  -- recolour the start and end positions

-- return a list of available movements for the current player
currentPlayerMovesList :: State GameTreeStatus [Transform]
currentPlayerMovesList = do eboard <- getBoard
                            ps <- getCurrentInternalBoard
                            colour <- getPlayerColour
                            let -- reverse the internal projected positions to the positions on the external board
                                -- faster than search occupied positions on the external board, since only the mathematical computation is necessary here
                                bs = ps `par` colour `pseq` map (appendColour colour . reversion colour) ps
                                -- the new positions could be reached from the above positions
                                ds = eboard `par` bs `pseq` evalState (do mapM destinationList bs) eboard
                            return (pairArrange bs ds) -- zip the resulting destinations with the start positions and generate a list of movement pairs

-- combine a list of start positions and lists of corresponding destinations into a list of movements: [(start, end)]
pairArrange :: [BoardPos] -> [[BoardPos]] -> [Transform]
pairArrange _ [] = []
pairArrange [] _ = []
pairArrange (b:bs) (d:ds) = zip (repeat b) d ++ pairArrange bs ds

-- return the current player's colour based on the number of players
playerColour :: PlayerIndex -> Int -> Colour
playerColour pi pn = playerColourList pn !! pi

-- edit a certain win for a node with an increment
editNodeValue :: PlayerIndex -> GameTree -> GameTree
editNodeValue _ (GRoot i ts) = GRoot i ts
editNodeValue pi (GLeaf i ft ws) = let newWins = replace pi ((ws!!pi)+1) ws -- replace the entity with a new value of an increment
                                   in  GLeaf i ft newWins
editNodeValue pi (GNode i ft ws ts) = let newWins = replace pi ((ws!!pi)+1) ws
                                      in  GNode i ft newWins ts

-- change the children for a node
editNodeChildren :: [GameTree] -> GameTree -> GameTree
editNodeChildren [] t = t
editNodeChildren ts (GRoot i _) = GRoot i ts
editNodeChildren ts (GNode i ft ws _) = GNode i ft ws ts
editNodeChildren ts (GLeaf i ft ws) = GNode i ft ws ts -- a leaf becomes an internal node when having at least one children 

--Node Evaluation-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- determine a node's profits based on different measurements， formulations

-- the average of how well a move/node is performing based on dividing the total wins with total visits
averageScore :: PlayerIndex -> Wins -> Int -> Double
averageScore _ _ 0 = 0
averageScore pi wins visits = fromIntegral (wins !! pi) / fromIntegral visits

-- determine a node's profits according to the UCT formula
-- consider the total visits of the parent node as well as the child node
estimateNodeUCT :: Int -> Int -> State GameTreeStatus Double
estimateNodeUCT pv 0 = return 0
estimateNodeUCT pv v = do c <- getUCTCons
                          return $ c * sqrt (log (fromIntegral pv) / fromIntegral v)

-- consider additionally the similar move played in the past, that is, the average score of similar move being performed
-- tree-only PH: only consider the moves selected in the selection stages
estimateNodePH :: Int -> Wins -> Int -> State GameTreeStatus Double
estimateNodePH ph gwins gvisits = do ht <- getHistoryTrace
                                     pi <- getPlayerIdx
                                     case rbSearch ph ht of -- find if similar move has been made in the past
                                        Nothing -> return 0
                                        Just hwins -> do w <- getPHCons
                                                         return $ fromIntegral (hwins !! pi) / fromIntegral (sum hwins)
                                                                  * w / fromIntegral (gvisits - (gwins !! pi) + 1)

-- when estimating a node during the selection, apply the above formula: Win Rate + UCT + PH 
estimateNode :: Int -> GameTree -> State GameTreeStatus Double
estimateNode pv node = if isRoot node then return 0
                       else do pi <- getPlayerIdx
                               colour <- getPlayerColour
                               ps <- modifyCurrentInternalBoard (getTransform node) -- first retrieve the new projected internal board from the transform
                               let wins = getWins node
                                   visits = sum wins
                                   mean = averageScore pi wins visits
                               uct <- estimateNodeUCT pv visits
                               ph  <- estimateNodePH (hash ps) wins visits -- then pass the hashed value of the internal board to the formula
                               return $ mean `par` uct `par` ph `pseq` mean + uct + ph


