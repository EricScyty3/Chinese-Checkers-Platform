module Experiment where
-- setup experimental trials for testing MCTS with different strategies

-- setup experimental trials for testing MCTS with different strategies
import MCTS ( getTurns, playout )
import GameTree
    ( turnBase,
      GameTree(GRoot),
      HistoryTrace,
      KillerMoves,
      PlayerIndex,
      PlayoutEvaluator (..),
      GameTreeStatus, getBoardIndex, BoardIndex)
import Board
import RBTree ( RBTree(RBLeaf) )
import Zobrist ( winStateDetect )
import System.Environment (getArgs)
import System.Random (newStdGen)
import Extension (finalSelection)
import Configuration (lookupTable)
import Data.Time (getCurrentTime)
import Data.Time.Clock (diffUTCTime)
import Control.Parallel (par)
import GHC.Conc (pseq)
import Data.Fixed (Pico)
import Data.List (elemIndices)
import Minimax (MGameTreeStatus)
import Control.Monad.State (evalState)
import System.Process

-- during the experimental trials, several outcomes are measured under the contorl of iteration counts and time limits
-- the first is the total win rate in the multiple players game, either only two player types are invovled or the same number of the total players
-- the second is the (median) playouts, or more specific, the iterations that could be performed under the set time
-- the thrid is the (median, average) turns that takes to end the game during the experiments
-- besides, the game is also limited to avoid endless cycle, and when this is occurred, the evalutor will be applied to determine the win for each player

-- retrieve the average value of a list
mean :: [Int] -> Double
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- generate the player arrangements for allowing multiple algorithms playing againts each other
validPlayerList :: Int -> Int -> [[Int]]
validPlayerList pn pt = filter (allPlayersInvolved [0 .. pt - 1]) (playerArrangement pn pt)
    where
        -- check if all player types are involved in the generated arrangement
        allPlayersInvolved :: [Int] -> [Int] -> Bool
        allPlayersInvolved [] _ = True
        allPlayersInvolved (p:ps) arrangement = p `elem` arrangement && allPlayersInvolved ps arrangement

        -- arrangements of players in several types of certain length
        -- could exist entities missing certain types
        playerArrangement :: Int -> Int -> [[Int]]
        playerArrangement 0 _ = [[]]
        playerArrangement players playerTypes = [x:xs | x <- [0 .. (playerTypes - 1)], xs <- playerArrangement (players - 1) playerTypes]

-- a list of players' settings to be invovled in the experimental trials, containing the evaluator to be used during the playout phase 
-- as well as the search depth
type PlayerList = [(PlayoutEvaluator, Int)]

-- given a list of player's settings, arrange them ensuring all players are involved in the game
generatePlayerList :: Int -> PlayerList -> [PlayerList]
generatePlayerList pn ps = let pt = length ps
                               pl = validPlayerList pn pt
                           in  map (findItemByIdx ps) pl

findItemByIdx :: [a] -> [Int] -> [a]
findItemByIdx _ [] = []
findItemByIdx ps (i:is) = ps !! i:findItemByIdx ps is

-- given a certain status, and run a game with different players till one of the players wins, and return the data for evaluating the performance
-- besides, one thing to point out is that the history trace and the killer moves are maintained by each player, in other words, they are not shared
singleRun :: [Board] -> (Maybe Int, Maybe Pico) -> PlayerIndex -> Board -> [[Pos]] -> Int -> [HistoryTrace] -> (Double, Double) -> [[KillerMoves]] -> PlayerList -> IO ()
singleRun record control pi eboard iboards pn hts cons kms pl =
                                    -- there might exist cycling where all players are trying to maintain a board state that is most benefical for them
                                    -- therefore, it is necessary to have a mechanism to break the loop 
                                    if getTurns len pn >= 500 then do printEoard eboard
                                                                      print $ length $ eboard `elemIndices` record
                                                                      --  print eboard
                                                                      error "Exceed!"
                                    else do gen <- newStdGen
                                            (neboard, niboard, nht, nkm) <- finalSelection (GRoot 0 []) (gen, pi, 1, eboard, iboards, pn, hts !! pi, cons, sim) control
                                            system "cls"
                                            printEoard neboard
                                            if winStateDetect niboard then return ()
                                            else let niboards = replace pi niboard iboards
                                                     nextTurn = turnBase pn pi
                                                 in  niboards `par` nextTurn `pseq`
                                                     singleRun (neboard:record) control nextTurn neboard niboards pn (replace pi nht hts) cons (replace pi nkm kms) pl
    where
        sim = let (x, y) = pl !! pi
              in  (x, y, kms !! pi)

        len = length record


-- start the game from the initial board state
runFromInitialState :: (Maybe Int, Maybe Pico) -> PlayerList -> IO ()
runFromInitialState control pl = singleRun [] control 0 eboard iboards pn hts (3, 1) kms pl
    where
        pn = length pl
        eboard = eraseBoard (playerColourList pn) externalBoard
        iboards = replicate pn startBase
        hts = replicate pn RBLeaf
        kms = replicate pn (replicate pn [])


-- run multiple rounds of the game with the same arrangement
-- multipleRuns :: Int -> (Maybe Int, Maybe Pico) -> PlayerList -> IO [(PlayerIndex, Int)]
-- multipleRuns 0 _ _ = return []
-- multipleRuns runs control ps = do item <- runFromInitialState control ps
--                                   items <- multipleRuns (runs - 1) control ps
--                                   return $ item:items

{-
multipleGames :: (Maybe Int, Maybe Pico) -> Int -> PlayerList -> IO ([(PlayoutEvaluator, Int)], [Int])
multipleGames control pn ps = let pl = generatePlayerList pn ps
                              in  do ls <- multipleGames' control pl
                                     let (winIdx, turns) = unzip ls
                                     return (findItemByIdx ps winIdx, turns)
    where                        
        multipleGames' :: (Maybe Int, Maybe Pico) -> [PlayerList] -> IO [(PlayerIndex, Int)]
        multipleGames' _ [] = return []
        multipleGames' control (p:ps) = do results <- runFromInitialState control p
                                           rest <- multipleGames' control ps
                                           return (results:rest)
-}
-- ghc -main-is Experiment Experiment.hs -O2 -outputdir dist
main :: IO ()
main = do arg <- getArgs
          start <- lookupTable `seq` getCurrentTime
          let iters = read (head arg)
        --       pn = 2 -- read (arg !! 1)
        --       ps = [(Move, 0), (Board, 0)] -- read (arg !! 2)
          runFromInitialState (Just iters, Nothing) [(Move, 0), (Board, 0)]
          end <- getCurrentTime
          putStrLn $ "Time cost: " ++ show (diffUTCTime end start)
{-
testBoard = 
    [[U (0,0),U (1,0),U (2,0),U (3,0),U (4,0),U (5,0),U (6,0),U (7,0),U (8,0),R (9,0),U (10,0),U (11,0),U (12,0),U (13,0),U (14,0),U (15,0),U (16,0),U (17,0),U (18,0)],[U (0,1),U (1,1),U (2,1),U (3,1),U (4,1),U (5,1),U (6,1),U (7,1),R (8,1),U (9,1),R (10,1),U (11,1),U (12,1),U (13,1),U (14,1),U (15,1),U (16,1),U (17,1),U (18,1)],[U (0,2),U (1,2),U (2,2),U (3,2),U (4,2),U (5,2),U (6,2),E (7,2),U (8,2),R (9,2),U (10,2),R (11,2),U (12,2),U (13,2),U (14,2),U (15,2),U (16,2),U (17,2),U (18,2)],[E (0,3),U (1,3),E (2,3),U (3,3),E (4,3),U (5,3),E (6,3),U (7,3),E (8,3),U (9,3),R (10,3),U (11,3),E (12,3),U (13,3),E (14,3),U (15,3),E (16,3),U (17,3),E (18,3)],[U (0,4),E (1,4),U (2,4),E (3,4),U (4,4),E (5,4),U (6,4),E (7,4),U (8,4),E (9,4),U (10,4),E (11,4),U (12,4),E (13,4),U (14,4),E (15,4),U (16,4),E (17,4),U (18,4)],[U (0,5),U (1,5),E (2,5),U (3,5),E (4,5),U (5,5),E (6,5),U (7,5),E (8,5),U (9,5),E (10,5),U (11,5),E (12,5),U (13,5),E (14,5),U (15,5),E (16,5),U (17,5),U (18,5)],[U (0,6),U (1,6),U (2,6),E (3,6),U (4,6),E (5,6),U (6,6),E (7,6),U (8,6),E (9,6),U (10,6),E (11,6),U (12,6),E (13,6),U (14,6),E (15,6),U (16,6),U (17,6),U (18,6)],[U (0,7),U (1,7),E (2,7),U (3,7),E (4,7),U (5,7),E (6,7),U (7,7),E (8,7),U (9,7),E (10,7),U (11,7),E (12,7),U (13,7),E (14,7),U (15,7),E (16,7),U (17,7),U (18,7)],[U (0,8),E (1,8),U (2,8),E (3,8),U (4,8),E (5,8),U (6,8),E (7,8),U (8,8),E (9,8),U (10,8),E (11,8),U (12,8),E (13,8),U (14,8),E (15,8),U (16,8),E (17,8),U (18,8)],[E (0,9),U (1,9),E (2,9),U (3,9),E (4,9),U (5,9),E (6,9),U (7,9),G (8,9),U (9,9),E (10,9),U (11,9),E (12,9),U (13,9),E (14,9),U (15,9),E (16,9),U (17,9),E (18,9)],[U (0,10),U (1,10),U (2,10),U (3,10),U (4,10),U (5,10),U (6,10),G (7,10),U (8,10),G (9,10),U (10,10),E (11,10),U (12,10),U (13,10),U (14,10),U (15,10),U (16,10),U (17,10),U (18,10)],[U (0,11),U (1,11),U (2,11),U (3,11),U (4,11),U (5,11),U (6,11),U (7,11),G (8,11),U (9,11),G (10,11),U (11,11),U (12,11),U (13,11),U (14,11),U (15,11),U (16,11),U (17,11),U (18,11)],[U (0,12),U (1,12),U (2,12),U (3,12),U (4,12),U (5,12),U (6,12),U (7,12),U (8,12),G (9,12),U (10,12),U (11,12),U (12,12),U (13,12),U (14,12),U (15,12),U (16,12),U (17,12),U (18,12)]]

testPs = [
    [(1,4),(0,4),(1,5),(0,5),(1,6),(0,6)],
    [(0,6),(1,6),(0,5),(1,5),(0,4),(1,4)]
    ]

testGameStatus :: IO GameTreeStatus
testGameStatus = do gen <- newStdGen
                    return (gen, 0, 0, testBoard, testPs, 2, RBLeaf, (3,0.9), (Board, 0, replicate 2 []))


testRun = do status <- testGameStatus
             let winIdx = evalState (playout 1) status
             print winIdx -}