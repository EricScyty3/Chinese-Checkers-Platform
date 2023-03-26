module Minimax where
-- the module of the shallow minimax search: Paranoid and BRS
import Board
import Control.Monad.State
import GameTree
import Configuration
import Control.Parallel
import Zobrist
import Data.Time
import Data.List
import System.Environment
import GHC.IO
import System.Random
import Control.Monad.Extra
import BFS (centroid)


-- the node is divided into two groups, Max for root layer and the rest is Min 
-- the search tree is also divided into two groups, the paranoid and BRS forms
data TreeType = Paranoid | BRS deriving (Eq, Show, Read)
-- the new tree status with less information to consider
type MGameTreeStatus = (-- Int, -- the current height of the layer
                        PlayerIndex, -- the root layer player's index, should be static
                        Board, -- the board state delivered from the parent
                        [[Pos]], -- the list of positions of the internal board for each player
                        Int, -- the total players, should be static
                        AlphaBeta,
                        TreeType -- the choice of shallow minimax search, should be static
                        )

type AlphaBeta = (Int, Int) -- the alpha and beta value, alhpa value represents the solution of Max node, and beta for Min node

-- the alpha and beta pair is not consistent throughout the search tree, it might be switched, for instance, 
-- the maximum value of the subtree could be the minimum value of its parent, when the layers of Max and Min are switched

-- ghc -main-is Minimax Minimax.hs -O2 -fllvm -outputdir dist
{-
main = do arg <- getArgs
          let searchDepth = read (head arg)
              treetype = read (arg !! 1)
          start <- getCurrentTime
          x <- runEnd searchDepth (status treetype) killerMoves 0 []
          printEoard x
          end <- getCurrentTime
          print $ diffUTCTime end start
    where
        pn = 3
        -- searchDepth = 3
        status tt = (0, eraseBoard (playerColourList pn) externalBoard, replicate pn startBase, pn, (-999, 999), tt)
        killerMoves = replicate pn []
-}

-- the win state detection here is not just checking the hash state, should consider not only the normal wining state, but also the potential blocking
-- a state is won for a player if its goal area is filled with pieces, and at least one of the pieces belongs it
winStateDetermine :: Colour -> Board -> Bool
winStateDetermine c b = let hs = map (reversion c) goalBase -- get the goal positions of certain player
                            bs = evalState (do mapM getElement hs) b -- get the corresponding board state
                            flag1 = isFull bs
                            flag2 = existColour c bs
                        in  flag1 `par` flag2 `pseq` flag1 && flag2 -- check if the two conditions are satisfied
    where
        isFull :: [BoardPos] -> Bool
        isFull = foldr ((&&) . isOccupied) True
        existColour :: Colour -> [BoardPos] -> Bool
        existColour c bs = Just c `elem` map Board.getColour bs

{-
runEnd :: Int -> MGameTreeStatus -> [KillerMoves] -> Int -> [Transform] -> IO Board
runEnd h st@(ri, eboard, iboards, pn, ab, tt) kms counts record =
                                                         if counts >= 500 then do print counts
                                                                                  print kms
                                                                                  return eboard
                                                         else
                                                         do let (move, nkms) = (if not (randomPercentage' 95) then (randomChoice, kms)
                                                                                else let ((generatedMove, _), updatedKms) = runState (mEvaluation h st ri) kms
                                                                                     in  (generatedMove, updatedKms))
                                                                checkedMove = (if move `elem` record then randomChoice else move)
                                                                neboard = repaintPath eboard checkedMove
                                                                niboard = flipBoard (iboards !! ri) (projectMove currentColour checkedMove)
                                                                nextTurn = turnBase pn ri
                                                            if  winStateDetermine currentColour neboard then do print counts
                                                                                                                print kms
                                                                                                                return neboard
                                                            else runEnd h (nextTurn, neboard, replace ri niboard iboards, pn, ab, tt) nkms (counts + 1) (checkedMove:record)

    where
        -- generate a random value from 0 to 100, for random percentage decision making
        randomPercentage' :: Int -> Bool
        randomPercentage' n = unsafePerformIO (do x <- randomRIO (0, 100); return (x <= n))
        -- generate a random index with a given length, for random choice
        randomMove' :: Int -> Int
        randomMove' l = unsafePerformIO $ do randomRIO (0, l-1)

        currentColour :: Colour
        currentColour = playerColour ri pn

        randomChoice :: Transform
        randomChoice = let ms = mplayerMovesList st ri
                           randIdx = randomMove' (length ms)
                       in  ms !! randIdx
-}

-- simple move evalutor of how close the changed piece is to the goal state, 
-- an addition move evaluator is to measure the forward distance to the goal base
dist :: Pos -> Pos -> Double
dist (x1, y1) (x2, y2) = sqrt (fromIntegral (x1 - x2)^2 + fromIntegral (y1 - y2)^2)
-- return the distance of a piece to the goal state, the evaluation is more straightforward as only considering the distance
moveEvaluation :: (Pos, Pos) -> Double
moveEvaluation (p1, p2) = let dist1 = dist p1 (0, 6)
                              dist2 = dist p2 (0, 6)
                           in dist1 `par` dist2 `pseq` (dist1 - dist2) -- still the larger the better

-- accept a list of movements and an distance-based heuristic, rank them from high to low
moveOrder :: [Transform] -> [Transform]
moveOrder ms = let result = assignDistance ms
                   sorted = sortBy (\(a, _) (b, _) -> compare b a) result
               in  map snd sorted
    where
        assignDistance :: [Transform] -> [(Double, Transform)]
        assignDistance [] = []
        assignDistance ((from, to):ms) = case getColour from of
                                        Nothing -> error "Invalid generated movement"
                                        Just co -> let internalPos = projectMove co (from, to)
                                                       dis = moveEvaluation internalPos -- transfer into the internal distance
                                                       item = (dis, (from, to))
                                                       items = assignDistance ms
                                                   in  item `par` items `pseq` item:items

kbestpruning :: [Transform] -> [Transform]
kbestpruning tfs = take 5 $ moveOrder tfs

-- search if there exist any killer moves at the current layer of minimax search
-- normally, the killer moves are specifically existing on certain ply, therefore, each ply should store its own killer moves
-- for simplicity, the killer moves list is having the same length as the total players, and indexing through the player index
-- since the target of killer moves is to save time, it should be tried first without considering any later computation

getCurrentKillerPair :: PlayerIndex -> State [KillerMoves] KillerMoves
getCurrentKillerPair pi = do ks <- get; return $ ks !! pi

replaceCurrentKillerPair :: PlayerIndex -> KillerMoves -> State [KillerMoves] ()
replaceCurrentKillerPair pi newk = do ks <- get; put (replace pi newk ks)

killerMoveTest :: KillerMoves -> [Transform] -> [Transform] -> ([Transform], [Transform])
killerMoveTest [] ms ss = (ss, ms)
killerMoveTest (k:ks) ms ss = case elemIndex k ms of
                                    Nothing  -> killerMoveTest ks ms ss
                                    Just idx -> killerMoveTest ks (removeByIdx ms idx) (ss ++ [k])

removeByIdx :: [a] -> Int -> [a]
removeByIdx xs idx = let front = take idx xs 
                         back = drop (idx + 1) xs
                     in  front `par` back `pseq` front ++ back

updateKillerMoves :: Transform -> KillerMoves -> KillerMoves
updateKillerMoves move km = take 2 $ nub $ move:km -- only keep several killer moves 

-- the pruning is available based on evaluating the nodes below
-- normally, when a Min node's (beta) value is larger than any parent (Max)'s alpha value, that branch can be pruned
-- on the other hand, when a Max node's (alpha) value is less than any parent (Min)'s beta value, the pruning can be applied as well
-- however, in a multi-player version, the switch of Max and Min nodes are not done layer by layer, 
-- for instance, a paranoid tree is having one Max layer and many of Min layers but the regular pruning could be applicable for Paranoid and BRS forms
-- besides, the return process is consistent and the Max node always returns alpha value while Min returns beta

--                  root          Given board state
--                 /    \
--                A      B        The new board states made by the root player
--               / \    / \
--              C   D  E   F      The new board states made by the first encountering player (one of the opponents), or all opponents
--             /     \/     \
--            G     ....     H    The follow up layers

-- given a depth and tree status, and the current player to start with, first generats its movements based on current board
-- and send it to evaluate the resulting boards from the movements
-- besides of returning the optimal score it could retrieve, it will also return the corresponding movement that generates that score
-- while digging the search tree (expanding the movements), in addition to the cut-off caused by the pruning, 
-- the search should also be stopped if one wins the game, in other hand, if the root player wins then 

mEvaluation :: Int -> MGameTreeStatus -> PlayerIndex -> State [KillerMoves] (Transform, Int)
mEvaluation 0 st pi = return (defaultMove, nEvaluation st pi)
mEvaluation height st@(ri, _, ps, pn, _, _) pi = do kp <- getCurrentKillerPair pi
                                                    let ms = mplayerMovesList st pi -- provide a list of possible movements that can be generated by the root layer
                                                        (indices, rmoves) = reorderMovements ms pn kp
                                                    if ri == pi then maxEvaluation (height - 1) st rmoves defaultMove indices
                                                    else minEvaluation (height - 1) st rmoves defaultMove indices

mEvaluation2 :: Int -> MGameTreeStatus -> [PlayerIndex] -> State [KillerMoves] (Transform, Int)
mEvaluation2 height st@(ri, _, ps, pn, _, _) indices =
                                                 do kp <- concatMapM getCurrentKillerPair indices
                                                    let ms = concatMap (mplayerMovesList st) indices -- provide a list of possible movements that can be generated by the root layer
                                                        (rindices, rmoves) = reorderMovements ms pn kp
                                                    if ri `elem` indices then maxEvaluation (height - 1) st rmoves defaultMove rindices
                                                    else minEvaluation (height - 1) st rmoves defaultMove rindices


-- given a list of avaliable movements, first take the existing killer moves away and reorder the movements based on distance increment
-- after that, append the found killer moves to the front of the list
reorderMovements :: [Transform] -> Int -> KillerMoves -> ([PlayerIndex], [Transform])
reorderMovements ms pn kms = let (appliedKms, remainMoves) = killerMoveTest kms ms []
                                 orderedMoves = kbestpruning remainMoves
                                 reorderedMove = appliedKms `par` orderedMoves `pseq` appliedKms ++ orderedMoves
                                 indices = assignIndex reorderedMove pn
                             in  (indices, reorderedMove)

assignIndex :: [Transform] -> Int -> [PlayerIndex]
assignIndex [] _ = []
assignIndex ((from, _):ms) pn = case getColour from of
                                    Nothing -> error "invalid movement"
                                    Just colour -> case colourIndex colour pn of
                                                    Nothing -> error "invalid colour"
                                                    Just index -> let indices = assignIndex ms pn
                                                                  in  index `par` indices `pseq` index:indices

-- the difference between two shallow minimax search: the Paranoid search follows the regular order base, while the BRS considers all players other than
-- the root as a layer, so it actually evaluates a list of boards from different players 
-- since it is not necessary to know about the best movement of the bottom layer, here it is ignored
treeSearch :: Int -> MGameTreeStatus -> PlayerIndex -> State [KillerMoves] Int
treeSearch 0 st pi = do (_, score) <- mEvaluation 0 st pi
                        return score
treeSearch h st@(ri, _, _, pn, _, Paranoid) pi = do (_, score) <- mEvaluation h st (turnBase pn pi)
                                                    return score
treeSearch h st@(ri, _, _, pn, _, BRS) pi = do (_, score) <- mEvaluation2 h st (turnBaseBRS pn ri pi)
                                               return score

-- the evaluation here will first sync the game status based on the given movements, and then passes it to the next layer until reaching the bottom (set depth)  
-- while maintaining the best score and the corresponding movement                                                       
maxEvaluation :: Int -> MGameTreeStatus -> [Transform] -> Transform -> [PlayerIndex] -> State [KillerMoves] (Transform, Int)
maxEvaluation _ (_, _, _, _, (alpha, _), _) [] bestMove _ = return (bestMove, alpha) -- Max layer returns alpha value
maxEvaluation _ (_, _, _, _, (alpha, _), _) _ bestMove [] = return (bestMove, alpha)
maxEvaluation height st@(ri, eboard, iboards, pn, ab@(alpha, beta), tt) (m:ms) bestMove (pi:pis) =
                                                        -- first update the status for the next layer
                                                        do let currentColour = playerColour pi pn
                                                               newiboard = flipBoard (iboards !! pi) (projectMove currentColour m)
                                                               newiboards = replace pi newiboard iboards
                                                               neweboard = repaintPath eboard m
                                                               nextTurnState = neweboard `par` newiboards `pseq` (ri, neweboard, newiboards, pn, ab, tt)
                                                           -- pass the status to the next layer and retrieve the evaluated result
                                                           score <- treeSearch height nextTurnState pi
                                                           kp <- getCurrentKillerPair pi
                                                           let (newBestMove, newAlpha) = if score >= alpha then (m, score) else (bestMove, alpha)
                                                           -- prune the branch if found a proof that the paranet won't choose this branch
                                                           if newAlpha >= beta then let newkp = updateKillerMoves m kp
                                                                                    in  do replaceCurrentKillerPair pi newkp -- update the killer moves when pruning is occurred
                                                                                           return (bestMove, beta)
                                                           else maxEvaluation height (ri, eboard, iboards, pn, (newAlpha, beta), tt) ms newBestMove pis
                                                                -- compute the next movement at the same layer

-- similar to above but different in the return value
minEvaluation :: Int -> MGameTreeStatus -> [Transform] -> Transform -> [PlayerIndex] -> State [KillerMoves] (Transform, Int)
minEvaluation _ (_, _, _, _, (_, beta), _) [] bestMove _ = return (bestMove, beta) -- Min layer returns beta value
minEvaluation _ (_, _, _, _, (_, beta), _) _ bestMove [] = return (bestMove, beta)
minEvaluation height st@(ri, eboard, iboards, pn, ab@(alpha, beta), tt) (m:ms) bestMove (pi:pis) =

                                                        do let currentColour = playerColour pi pn
                                                               newiboard = flipBoard (iboards !! pi) (projectMove currentColour m)
                                                               newiboards = replace pi newiboard iboards
                                                               neweboard = repaintPath eboard m
                                                               nextTurnState = neweboard `par` newiboards `pseq` (ri, neweboard, newiboards, pn, ab, tt)

                                                           score <- treeSearch height nextTurnState pi
                                                           kp <- getCurrentKillerPair pi
                                                           let (newBestMove, newBeta) = if score <= beta then (m, score) else (bestMove, beta) -- update the beta value

                                                           if alpha >= newBeta then let newkp = updateKillerMoves m kp
                                                                                    in  do replaceCurrentKillerPair pi newkp
                                                                                           return (bestMove, alpha)
                                                           else minEvaluation height (ri, eboard, iboards, pn, (alpha, newBeta), tt) ms newBestMove pis

-- when it comes to BRS, the search tree becomes different where the second layer's node is no longer one opponent, but all opponents
otherPlayers :: Int -> PlayerIndex -> [PlayerIndex]
otherPlayers pn ri = filter (/=ri) [0 .. pn - 1]
-- therefore, it needs a different turn switching mechanism
turnBaseBRS :: Int -> PlayerIndex -> PlayerIndex -> [PlayerIndex]
turnBaseBRS pn ri pi = if ri /= pi then [ri] else otherPlayers pn ri

-- evaluate the bottom node (where the depth is equal to 0) of the search tree
-- since the evaluation should be made based on the root's perspective, the evaluation of the other layer player is done by evaluating how the root player could benefit
nEvaluation :: MGameTreeStatus -> PlayerIndex -> Int -- only care about the root layer's score, make the win determination softer
nEvaluation st@(ri, eboard, iboard, pn, _, _) pi
    | winStateDetermine rolour eboard = 28 -- if root player wins, then return the maximum gain
    | ri == pi = head $ boardEvaluations [iboard !! ri] -- if no player winning, then just evaluate normally based on board evaluator
                 -- centroid (iboard !! ri)
    | otherwise = let ms = mplayerMovesList st ri -- for other players, how a board is evaluated is based on how the root player could play on this board   
                      rs = iboard !! ri
                      nbs = ms `par` rs `pseq` map (flipBoard rs . projectMove rolour) ms
                      scores = {-map centroid nbs-} boardEvaluations nbs
                  in  if null scores then error (show eboard) else maximum scores
    where
        rolour = playerColour ri pn

-- the enhancements applied here should include the: move ordering + k best pruning + killer moves for each layer
-- given a certain piece's colour, return a list of avaliable movements (transforms) on the external board
mplayerMovesList :: MGameTreeStatus -> PlayerIndex -> [Transform]
mplayerMovesList (_, eboard, iboard, pn, _, _) pi = let colour = playerColour pi pn -- the colour of the player's piece
                                                        ps = (iboard !! pi) -- the internal positions of a player
                                                        bs = colour `par` ps `pseq` map (appendColour colour . reversion colour) ps -- project those position onto the external board
                                                        ds = evalState (do mapM destinationList bs) eboard -- and generate movements based on the external board state
                                                    in  pairArrange bs ds -- zip the start and the end, ensuring that those are listed in pairs


