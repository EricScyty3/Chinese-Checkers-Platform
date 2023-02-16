module Minimax where
-- the module of the shallow minimax search: Paranoid and BRS
import Board
import Control.Monad.State
import GameTree
import Configuration (boardEvaluation)
import Control.Parallel
import Zobrist

-- the search tree structure for minimax search, since it is applied to multi-player search that divides players into 2
-- the node only records the score for the root layer
data MGameTree = MRoot [MGameTree] |
                 MLeaf Transform Int  |
                 MNode Transform Int [GameTree]
                 deriving (Eq, Show)
-- the node is divided into two groups, Max for root layer and the rest is Min 
-- data NodeType = Max | Min deriving (Eq, Show)
-- the search tree is also divided into two groups, the paranoid and BRS forms
data TreeType = Paranoid | BRS deriving (Eq, Show)
-- the new tree status with less information to consider
type MGameTreeStatus = (PlayerIndex, -- the root player's index
                        PlayerIndex, -- current player of the turn
                        Board, -- the board state delivered from the parent
                        [[Pos]], -- the list of positions of the internal board for each player
                        Int, -- the total players
                        Int -- the depth of the search tree
                        )

{-
-- given a change of board, create a leaf node from a parent
makeMLeaf :: Transform -> State MGameTreeStatus MGameTree
makeMLeaf tf = do ps <- modifyCurrentMInternalBoard tf
                  -- boardEvaluation
                  return undefined
                     -- return (MLeaf tf (replicate pn 0)) -- allocate the board index and no children as it's a leaf 
-}
rootEvaluation :: Transform -> State MGameTreeStatus Int
rootEvaluation tf = do pi <- getMPlayerIndex
                       ps <- modifyMInternalBoard pi tf
                       return $ boardEvaluation ps            

-- given a certain piece's colour, return a list of avaliable movements (transforms)
mcolouredMovesList :: PlayerIndex -> State MGameTreeStatus [Transform]
mcolouredMovesList idx = do board <- getMBoard
                            psL <- getMInternalBoard 
                            pn <- getMTotoalPlayer
                            let ps = psL !! idx
                                colour = playerColour idx pn
                                bs = ps `par` colour `pseq` map (appendColour colour . reversion colour) ps -- invert the internal positions to external ones
                                ds = evalState (do mapM destinationList bs) board -- the new positions resulting from moving the above pieces
                            return (pairArrange bs ds) -- zip the resulting movements with the current pieces

-- retrieve the status
getMRoot :: State MGameTreeStatus PlayerIndex
getMRoot = do (rootIndex, _, _, _, _, _) <- get; return rootIndex
getMBoard :: State MGameTreeStatus Board
getMBoard = do (_, _, board, _, _, _) <- get; return board
getMPlayerIndex :: State MGameTreeStatus PlayerIndex
getMPlayerIndex = do (_, idx, _, _, _, _) <- get; return idx
getMPlayerColour :: PlayerIndex -> State MGameTreeStatus Colour
getMPlayerColour pi = do playerColour pi <$> getMTotoalPlayer
getMInternalBoard :: State MGameTreeStatus [[Pos]]
getMInternalBoard = do (_, _, _, positions, _, _) <- get; return positions
getCurrentMInternalBoard :: State MGameTreeStatus [Pos]
getCurrentMInternalBoard = do pi <- getMPlayerIndex
                              ps <- getMInternalBoard
                              return (ps !! pi)
getMTotoalPlayer :: State MGameTreeStatus Int
getMTotoalPlayer = do (_, _, _, _, playerNumber, _) <- get; return playerNumber
getMDepth :: State MGameTreeStatus Int
getMDepth = do (_, _, _, _, _, depth) <- get; return depth
-- update the status
setMBoard :: Board -> State MGameTreeStatus ()
setMBoard board = do (ri, idx, _, ps, pn, dp) <- get; put (ri, idx, board, ps, pn, dp)
setMInternalBoard :: [[Pos]] -> State MGameTreeStatus ()
setMInternalBoard ps = do (ri, idx, board, _, pn, dp) <- get; put (ri, idx, board, ps, pn, dp)
modifyMInternalBoard :: PlayerIndex -> Transform -> State MGameTreeStatus [Pos]
modifyMInternalBoard pi (from, to) = do ps <- getCurrentMInternalBoard
                                        colour <- getMPlayerColour pi
                                        let pf = projection colour (getPos from) 
                                            pt = projection colour (getPos to)
                                            newps = pf `par` pt `pseq` flipBoard ps (pf, pt)
                                        return newps 
updateCurrentMInternalBoard :: PlayerIndex -> [Pos] -> State MGameTreeStatus ()
updateCurrentMInternalBoard pi ps = do psL <- getMInternalBoard
                                       let npsL = replace pi ps psL
                                       setMInternalBoard npsL
-- standard minimax search 
-- advanced version with pruning
-- appended with several enhancements
-- multi-player mode

-- -- given a depth, generate a tree from a given board state
-- createParanoidTree :: Int -> State MGameTreeStatus MGameTree
-- createParanoidTree depth = do let root = MRoot Max []
--                               board <- getMBoard
--                               colour <- getMPlayerColour
--                               tfs <- colouredMovesList colour

-- expandNode :: MGameTree -> State MGameTreeStatus MGameTree
-- expandNode node = do colour <- getMPlayerColour
--                      tfs <- colouredMovesList colour
--                      cs <- mapM makeLeaf (expandPolicy co ts)


