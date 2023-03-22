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
type KillerMoves = [Transform] -- the two last moves that were best or caused a cutoff at the current layer

-- the alpha and beta pair is not consistent throughout the search tree, it might be switched, for instance, 
-- the maximum value of the subtree could be the minimum value of its parent, when the layers of Max and Min are switched

-- ghc -main-is Minimax Minimax.hs -O2 -fllvm -outputdir dist

main = do arg <- getArgs
          let pn = read (head arg)
          start <- getCurrentTime
          x <- runEnd searchDepth (status pn) (killerMoves pn) 0 []
          printEoard x
          end <- getCurrentTime
          print $ diffUTCTime end start
    where 
        -- pn = 3
        searchDepth = 2
        status pn = (0, eraseBoard (playerColourList pn) externalBoard, replicate pn startBase, pn, (-999, 999), Paranoid)
        killerMoves pn = replicate pn []


winStateDetermine' :: Colour -> Board -> Bool
winStateDetermine' c b = let hs = map (reversion c) goalBase -- get the goal positions of certain player
                             bs = evalState (do mapM getElement hs) b -- get the corresponding board state
                         in  isFull bs && existColour c bs -- check if the two conditions are satisfied
    where
        isFull :: [BoardPos] -> Bool
        isFull = foldr ((&&) . isOccupied) True
        existColour :: Colour -> [BoardPos] -> Bool
        existColour c bs = Just c `elem` map Board.getColour bs



runEnd :: Int -> MGameTreeStatus -> [KillerMoves] -> Int -> [Transform] -> IO Board
runEnd h st@(ri, eboard, iboards, pn, ab, tt) kms counts record =
                                                         do let (move, nkms) = (if not (randomPercentage' 95) then (randomChoice, kms)
                                                                                else let ((generatedMove, _), updatedKms) = runState (mEvaluation h st ri) kms
                                                                                     in  (generatedMove, updatedKms))
                                                                checkedMove = (if move `elem` record then randomChoice else move) 
                                                                neboard = repaintPath eboard checkedMove
                                                                niboard = flipBoard (iboards !! ri) (projectMove currentColour checkedMove)
                                                                nextTurn = turnBase pn ri
                                                            if winStateDetermine' currentColour neboard then do print counts
                                                                                                                return neboard
                                                            else runEnd h (nextTurn, neboard, replace ri niboard iboards, pn, ab, tt) nkms (counts + 1) (checkedMove:record)

    where
        -- generate a random value from 0 to 100, for random percentage decision making
        randomPercentage' :: Int -> Bool
        randomPercentage' n = unsafePerformIO (do x <- randomRIO (0, 100); return (x <= n))
        -- generate a random index with a given length, for random choice
        randomMove' :: Int -> Int
        randomMove' l = unsafePerformIO $ do randomRIO (0, l-1)
        currentColour = playerColour ri pn

        randomChoice :: Transform
        randomChoice = let ms = mplayerMovesList st ri
                           randIdx = randomMove' (length ms)  
                       in  ms !! randIdx

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
moveOrder :: Colour -> [Transform] -> [Transform]
moveOrder co ms =
                let pms = map (projectMove co) ms
                    dis = map moveEvaluation pms -- transfer into the internal distance
                    pairs = zip dis ms
                    sortedList = sortBy (\(a, _) (b, _) -> compare b a) pairs
                in  map snd sortedList

-- accept multiple lists of movements, sort them based on the distance increment
-- moveOrder2 :: [Colour] -> [[Transform]] -> [Transform]
-- moveOrder2 cs ms = let sortedList = sortBy (\(a, _) (b, _) -> compare b a) (moveOrder2' cs ms)
--                    in  map snd sortedList
--     where
--         moveOrder2' :: [Colour] -> [[Transform]] -> [(Double, Transform)]
--         moveOrder2' [] _ = []
--         moveOrder2' _ [] = []
--         moveOrder2' (c:cs) (m:ms) = let ps = map (projectMove c) m
--                                         ds = map moveEvaluation ps
--                                         pairs = zip ds m
--                                         otherPairs = moveOrder2' cs ms
--                                     in  pairs `par` otherPairs `pseq` (pairs ++ otherPairs)

kbestpruning :: Colour -> [Transform] -> [Transform]
kbestpruning colour tfs = take 5 $ moveOrder colour tfs

-- kbestpruning2 :: [Colour] -> [[Transform]] -> [Transform]
-- kbestpruning2 cs ms = take 5 $ moveOrder2 cs ms

-- search if there exist any killer moves at the current layer of minimax search
-- if it does, then we have to put it to the front of the sorted list such that them will be tried first
-- normally, the killer moves are specifically existing on certain ply, therefore, each ply should store its own killer moves
-- besides, since the killer moves are specific for certain layer, if wanting to reuse those moves, it will be better to modify it for the next iteration, as the game progresses also push the layer of the search
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

-- killerMoveTest2 :: [KillerMoves] -> [[Transform]] -> [Transform] -> [[Transform]] -> ([Transform], [[Transform]])
-- killerMoveTest2 [] _ ss ns = (ss, ns)
-- killerMoveTest2 _ [] ss ns = (ss, ns)
-- killerMoveTest2 (k:ks) (m:ms) ss ns = let (selected, remained) = killerMoveTest k m ss 
--                                       in  killerMoveTest2 ks ms (ss ++ selected) (ns ++ [remained])

removeByIdx :: [a] -> Int -> [a]
removeByIdx xs idx = take idx xs ++ drop (idx + 1) xs

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
                                                        reorderedMoves = reorderMovements ms kp (playerColour pi pn)
                                                    if ri == pi then maxEvaluation (height - 1) st pi reorderedMoves defaultMove
                                                    else minEvaluation (height - 1) st pi reorderedMoves defaultMove

defaultMove :: Transform
defaultMove = (U(-1,-1), U(-1,-1))

-- for the BRS variant, since it detects all player other than the root, it will require specific enhancement
-- multiplyReorder :: [[Transform]] -> [KillerMoves] -> [Colour] -> [Transform]
-- multiplyReorder ms ks cs = let (appliedKms, remainList) = killerMoveTest2 ks ms [] []
--                                orderedMoves = kbestpruning2 cs remainList
--                            in  appliedKms ++ orderedMoves

reorderMovements :: [Transform] -> KillerMoves -> Colour -> [Transform]
reorderMovements ms kms colour = let (appliedKms, remainMoves) = killerMoveTest kms ms []
                                     orderedMoves = kbestpruning colour remainMoves
                                 in  appliedKms ++ orderedMoves

-- the difference between two shallow minimax search: the Paranoid search follows the regular order base, while the BRS considers all players other than
-- the root as a layer, so it actually evaluates a list of boards from different players 
-- since it is not necessary to know about the best movement of the next layer, here it is ignored

treeSearch :: Int -> MGameTreeStatus -> PlayerIndex -> State [KillerMoves] [Int]
treeSearch 0 st pi = do (_, score) <- mEvaluation 0 st pi
                        return [score]
treeSearch h st@(ri, _, _, pn, _, Paranoid) pi = do (_, score) <- mEvaluation h st (turnBase pn pi)
                                                    return [score]
treeSearch h st@(ri, _, _, pn, _, BRS) pi = do ls <- mapM (mEvaluation h st) (turnBaseBRS pn ri pi)
                                               let scores = map snd ls
                                               return scores

-- the evaluation here will first sync the game status based on the given movements, and then passes it to the next layer until reaching the bottom (set depth)  
-- while maintaining the best score and the corresponding movement                                                               
maxEvaluation :: Int -> MGameTreeStatus -> PlayerIndex -> [Transform] -> Transform -> State [KillerMoves] (Transform, Int)
maxEvaluation _ (_, _, _, _, (alpha, _), _) _ [] bestMove = return (bestMove, alpha) -- Max layer returns alpha value
maxEvaluation height st@(ri, eboard, iboards, pn, ab@(alpha, beta), tt) pi (m:ms) bestMove =
                                                        
                                                        do let currentColour = playerColour pi pn
                                                               newiboard = flipBoard (iboards !! pi) (projectMove currentColour m)
                                                               newiboards = replace pi newiboard iboards
                                                               neweboard = repaintPath eboard m
                                                               nextTurnState = (ri, neweboard, newiboards, pn, ab, tt)
                                                           
                                                           scores <- treeSearch height nextTurnState pi
                                                           kp <- getCurrentKillerPair pi
                                                           let maxScore = maximum scores
                                                               (newBestMove, newAlpha) = if maxScore >= alpha then (m, maxScore) else (bestMove, alpha)
                                                           
                                                           if newAlpha >= beta then -- prune the branch if found a proof that the paranet won't choose this branch
                                                                                    let newkp = updateKillerMoves m kp
                                                                                    in  do replaceCurrentKillerPair pi newkp
                                                                                           return (bestMove, beta)
                                                           else maxEvaluation height (ri, eboard, iboards, pn, (newAlpha, beta), tt) pi ms newBestMove
                                                        
                                                        --     
                                                        --     
                                                        --     -- digging down
                                                        --     nextTurnPlayer = turnBase pn mi
                                                        --     nextTurnMoves = mplayerMovesList nextTurnState nextTurnPlayer

                                                        -- kpm <- getCurrentKillerPair mi
                                                        -- kpn <- getCurrentKillerPair nextTurnPlayer
                                                        -- let reorderedList = reorderMovements nextTurnPlayer nextTurnMoves kpn (playerColour nextTurnPlayer pn)

                                                        -- -- let reorderList = killerMoveTest kp1 newMovesList
                                                        -- (_, score) <- minEvaluation nextTurnState nextTurnPlayer reorderedList defaultMove
                                                        -- -- updating the current state
                                                        -- let (newBestMove, newAlpha) = if score >= alpha then (m, score) else (bestMove, alpha)
                                                        -- if newAlpha >= beta then do let newkpm = updateKillerMoves m kpm
                                                        --                             replaceCurrentKillerPair mi newkpm
                                                        --                             return (bestMove, beta) -- prune
                                                        -- else maxEvaluation height (ri, board, pl, pn, (newAlpha, beta), tt) mi ms newBestMove

                                                                                -- do -- kp <- getCurrentKillerPair height
                                                                                --    let nib = flipBoard (pl !! pi) (projectMove (playerColour pi pn) m) -- edit the internal positions
                                                                                --        npl = replace pi nib pl
                                                                                --        neb = repaintPath board m -- edit the new board state
                                                                                --        nst = (ri, neb, npl, pn, ab, tt) -- retrieve new tree status
                                                                                --        npi = turnBase pn pi
                                                                                --    score <- minEvaluation nst npi ()  -- treeSearch nst pi -- decide in which way keep (Paranoid or BRS style) digging down

                                                                                --    let maxScore = maximum score
                                                                                --        -- and calculate the maximum score
                                                                                --        (newBestMove, newAlpha) = if maxScore >= alpha then (m, maxScore) else (bestMove, alpha) -- update the alpha value and the best move so far
                                                                                --    if newAlpha >= beta then -- prune the branch if found a proof that the paranet won't choose this branch
                                                                                --                             -- let newkp = updateKillerMoves m kp
                                                                                --                             -- in  do replaceCurrentKillerPair height newkp
                                                                                --                                    return (bestMove, beta)
                                                                                --    else maxEvaluation (ri, board, pl, pn, (newAlpha, beta), tt) pi ms newBestMove -- get to the next movements provided in a list


-- similar to above but different in the return value
minEvaluation :: Int -> MGameTreeStatus -> PlayerIndex -> [Transform] -> Transform -> State [KillerMoves] (Transform, Int)
minEvaluation _ (_, _, _, _, (_, beta), _) _ [] bestMove = return (bestMove, beta) -- Min layer returns beta value
minEvaluation height st@(ri, eboard, iboards, pn, ab@(alpha, beta), tt) pi (m:ms) bestMove =
                                                        do let currentColour = playerColour pi pn
                                                               newiboard = flipBoard (iboards !! pi) (projectMove currentColour m)
                                                               newiboards = replace pi newiboard iboards
                                                               neweboard = repaintPath eboard m
                                                               nextTurnState = (ri, neweboard, newiboards, pn, ab, tt)
                                                           
                                                           scores <- treeSearch height nextTurnState pi
                                                           kp <- getCurrentKillerPair pi
                                                           let minScore = minimum scores
                                                               (newBestMove, newBeta) = if minScore <= beta then (m, minScore) else (bestMove, beta) -- update the beta value
                                                           
                                                           if alpha >= newBeta then -- since the pruning is occured here, the killer moves need to be updated
                                                                                    let newkp = updateKillerMoves m kp
                                                                                    in  do replaceCurrentKillerPair pi newkp
                                                                                           return (bestMove, alpha)
                                                           else minEvaluation height (ri, eboard, iboards, pn, (alpha, newBeta), tt) pi ms newBestMove
                                                                                --  do -- kp <- getCurrentKillerPair height
                                                                                --     let nib = flipBoard (pl !! pi) (projectMove (playerColour pi pn) m) -- new internal board based on the current movement
                                                                                --         npl = replace pi nib pl -- update the internal board list
                                                                                --         neb = repaintPath board m -- update the external board by applying the movement
                                                                                --         nst = (ri, neb, npl, pn, ab, tt) -- new game status
                                                                                --         nextPlayerIndex = turnBase pn pi -- the next turn's player
                                                                                --         nextTurnMoves = mplayerMovesList nst nextPlayerIndex
                                                                                --     score <- treeSearch nst pi
                                                                                --     let minScore = minimum score
                                                                                --         (newBestMove, newBeta) = if minScore <= beta then (m, minScore) else (bestMove, beta) -- update the beta value
                                                                                --     if alpha >= newBeta then -- since the pruning is occured here, the killer moves need to be updated
                                                                                --                             -- let newkp = updateKillerMoves m kp
                                                                                --                             -- in  do replaceCurrentKillerPair height newkp
                                                                                --                                    return (bestMove, alpha)
                                                                                --     else minEvaluation (ri, board, pl, pn, (alpha, newBeta), tt) pi ms newBestMove


-- when it comes to BRS, the search tree becomes different where the second layer's node is no longer one opponent, but all opponents
otherPlayers :: Int -> PlayerIndex -> [PlayerIndex]
otherPlayers pn ri = filter (/=ri) [0 .. pn - 1]
-- therefore, it needs a different turn switching mechanism
turnBaseBRS :: Int -> PlayerIndex -> PlayerIndex -> [PlayerIndex]
turnBaseBRS pn ri pi = if ri /= pi then [ri] else otherPlayers pn ri

-- evaluate the bottom node (where the depth is equal to 0) of the search tree
-- since the evaluation should be made based on the root's perspective, the evaluation of the other layer player is done by evaluating how the root player could benefit
-- nEvaluation :: MGameTreeStatus -> PlayerIndex -> Int
-- nEvaluation st@(h, ri, _, iboard, pn, _, _) pi = 
--     if ri == pi then head $ boardEvaluations [iboard !! ri] -- for root, just simply measure its score
--     else let rolour = playerColour ri pn -- for other players, measure the possible root player's move could perform based on the changed board
--              rs = iboard !! ri
--              tfs = mplayerMovesList st ri -- the avaliable root's movements (pruned)
--              -- ms = take 10 $ moveOrder rolour tfs
--              bs = map (flipBoard rs . projectMove rolour) tfs -- the resulting boards that could generated by the root player
--              scores = boardEvaluations bs -- the corresponding score
--          in  if null scores then 28 else maximum scores -- return the maximum one

nEvaluation :: MGameTreeStatus -> PlayerIndex -> Int -- only care about the root layer's score, make the win determination softer
nEvaluation st@(ri, eboard, iboard, pn, _, _) pi
    | winStateDetermine' rolour eboard = 28
    | winStateDetermine' polour eboard = 0
    | ri == pi = head $ boardEvaluations [iboard !! ri]
    | otherwise = let ms = mplayerMovesList st ri
                      rs = iboard !! ri
                      nbs = map (flipBoard rs . projectMove rolour) ms
                      scores = boardEvaluations nbs
                  in  if null scores then error (show eboard) else maximum scores
    where
        rolour = playerColour ri pn
        polour = playerColour pi pn

--   | ri == pi = head $ boardEvaluations [iboard !! ri] -- if it is root player's turn, then just measure the board state
-- --   | winStateDetect (iboard !! pi) && not (winStateDetect (iboard !! ri)) = 0 -- if other player wins the game then root player gains 0
-- --   | winStateDetect (iboard !! ri) = 28 -- if root player wins, then it gains 28
--   | otherwise = let rolour = playerColour ri pn  -- for other player's turn, measure the best possible root player's move based on the board
--                     rs = iboard !! ri
--                     tfs = mplayerMovesList st ri -- the avaliable root's movements (pruned)
--                     bs = map (flipBoard rs . projectMove rolour) tfs -- the resulting boards that could generated by the root player
--                     scores = boardEvaluations bs -- the corresponding score
--                 in  maximum scores

-- the enhancements applied here should include the: move ordering + k best pruning + killer moves for each layer
-- given a certain piece's colour, return a list of avaliable movements (transforms) on the external board
mplayerMovesList :: MGameTreeStatus -> PlayerIndex -> [Transform]
mplayerMovesList (_, eboard, iboard, pn, _, _) pi =
                                                    let colour = playerColour pi pn -- the colour of the player's piece
                                                        ps = (iboard !! pi) -- the internal positions of a player
                                                        bs = map (appendColour colour . reversion colour) ps -- project those position onto the external board
                                                        ds = evalState (do mapM destinationList bs) eboard -- and generate movements based on the external board state
                                                    in  pairArrange bs ds -- zip the start and the end, ensuring that those are listed in pairs


