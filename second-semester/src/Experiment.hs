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
    ( eraseBoard,
      externalBoard,
      playerColourList,
      printEoard,
      replace,
      startBase,
      Board,
      Pos, removeByIdx, goalBase )
import RBTree ( RBTree(RBLeaf) )
import Zobrist ( winStateDetect )
import System.Environment (getArgs)
import System.Random (newStdGen)
import Extension (finalSelection, MCTSControl)
import Configuration (lookupTable, boardEvaluations)
import Data.Time (getCurrentTime)
import Data.Time.Clock (diffUTCTime)
import Control.Parallel (par, pseq)
import Data.Fixed (Pico)
import Data.List (elemIndices, sort, nub, permutations, elemIndex)
import Minimax (MGameTreeStatus)
import Control.Monad.State (evalState, replicateM)
import Data.List.Extra (chunksOf)
import System.IO ( hClose, openFile, hPutStr, IOMode(WriteMode, ReadMode), hGetContents )
import System.Process (system)
import Text.Printf (printf)
import Control.Concurrent.Async ( mapConcurrently )
import Data.Maybe (fromMaybe, isNothing)

-- import Data.List.Split
-- import Control.Parallel.Strategies (using, parList, rseq, parListChunk, parMap)

-- during the experimental trials, several outcomes are measured only under the contorl of time limits
-- the first is the total win rate in the multiple players game, either only two player types are invovled or the same number of the total players
-- the second is the (median) playouts, or more specific, the iterations that could be performed under the set time

-- a list of players' settings to be invovled in the experimental trials, containing the evaluator to be used during the playout phase 
-- as well as the search depth
type Player = (PlayoutEvaluator, Int)

-- retrieve the average value of a list
mean :: [Int] -> Double
mean [] = 0
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

median :: [Int] -> Double
median xs = median' (sort xs)
    where
        median' :: [Int] -> Double
        median' [] = 0
        median' [x] = fromIntegral x
        median' [x, y] = fromIntegral (x + y) / 2
        median' (x:xs) = median' (init xs)

-- generate the player arrangements for allowing multiple algorithms playing againts each other
validPlayerList :: Int -> Int -> [[Int]]
validPlayerList pn pt = filter (not . samePlayers) (playerArrangement pn pt)
    where
        -- check if all player types are the same in the generated arrangements
        samePlayers :: [Int] -> Bool
        samePlayers [] = True
        samePlayers [_] = True
        samePlayers (x:y:zs) = x == y && samePlayers (y:zs)

        -- arrangements of players in several types of certain length
        -- could exist entities missing certain types
        playerArrangement :: Int -> Int -> [[Int]]
        playerArrangement 0 _ = [[]]
        playerArrangement players playerTypes = [x:xs | x <- [0 .. (playerTypes - 1)], xs <- playerArrangement (players - 1) playerTypes]

-- based on the valid assignments given, only select the ones with two players involved
-- might be useful if wanting to dig deeper on the performance
tournamentList :: Int -> Player -> Player -> [[Player]]
tournamentList pn p1 p2 = let pl = validPlayerList pn 2
                          in  map (`binaryMap` (p1, p2)) pl
    where
        binaryMap [] _ = []
        binaryMap (x:xs) (p1, p2) = (if x == 0 then p1 else p2):binaryMap xs (p1, p2)

-- given a list of players, arrange them in several different orders
-- True for multiple player types, and False for only two players involved
generatePlayerList :: Int -> [Player] ->[[Player]]
generatePlayerList pn ps = let pt = length ps
                               pl = validPlayerList pn pt
                           in  map (findItemByIdx ps) pl
    where
        findItemByIdx :: [a] -> [Int] -> [a]
        findItemByIdx _ [] = []
        findItemByIdx ps (i:is) = ps !! i:findItemByIdx ps is

--Run Experimental Trials---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- given a player, run the playout phase from the initial board till the end of the game
-- this is applied to see the search speed of each evaluator 
runSimulation :: Player -> IO Double
runSimulation (evaluator, depth) = do start <- getCurrentTime
                                      newGen <- newStdGen
                                      -- the position of the player is not important here, so it is fixed to 0
                                      let winIdx = start `seq` evalState (playout 0) (newGen, 0, 1, eboard, iboards, pn, RBLeaf, (0.5, 5), (evaluator, depth, kms))
                                      end <- winIdx `seq` getCurrentTime
                                      return $ realToFrac $ end `seq` diffUTCTime end start

    where
        pn = 3
        eboard = eraseBoard (playerColourList pn) externalBoard
        iboards = replicate pn startBase
        kms = replicate pn []

-- run the simulation several times from the initial board state
runMultipleSimulations :: Int -> Player -> IO Double
runMultipleSimulations runs player = let pls = replicate runs player
                                     in  do durations <- mapM runSimulation pls
                                            return $ sum durations

-- given a certain status, and run a game with different players till one of the players wins, and return the data for evaluating the performance
-- besides, one thing to point out is that the history trace and the killer moves are maintained by each player, in other words, they are not shared
singleRun :: MCTSControl -> PlayerIndex -> Board -> [[Pos]] -> Int -> [HistoryTrace] ->
             (Double, Double) -> [[KillerMoves]] -> [Player] -> Int -> IO (Maybe PlayerIndex)
singleRun control pi eboard iboards pn hts cons kms pl counts =
                                    -- there might exist cycling where all players are trying to maintain a board state that is most benefical for them
                                    -- therefore, it is necessary to have a mechanism to break the loop 
                                    if getTurns counts pn >= 500 then return Nothing
                                    else do gen <- newStdGen
                                            (neboard, niboard, nht, nkm) <- finalSelection (GRoot 0 []) (gen, pi, 1, eboard, iboards, pn, hts !! pi, cons, sim) control
                                            let newHistory = replace pi nht hts
                                                newKillerMoves = replace pi nkm kms
                                            -- system "cls"
                                            -- printEoard neboard
                                            if winStateDetect niboard then return $ Just pi
                                            else let niboards = replace pi niboard iboards
                                                     nextTurn = turnBase pn pi
                                                 in  niboards `par` nextTurn `pseq`
                                                     singleRun control nextTurn neboard niboards pn newHistory cons newKillerMoves pl (counts+1)
    where
        sim = let (x, y) = pl !! pi
              in  (x, y, kms !! pi)


-- start the game from the initial board state
-- from here, one phenomenon could be found, that the performed playouts were increasing as the game progressing, this could be because that when the game is close to 
-- the end state, there are no many effective move can be played, therefore, an iteration is stopped very fast and lead to an increasing number of playouts
runFromInitialState :: MCTSControl -> [Player] -> IO Player
runFromInitialState control pl = do result <- singleRun control 0 eboard iboards pn hts (0.5, 5) kms pl 0
                                    case result of
                                        Nothing -> runFromInitialState control pl -- rerun the experiment if cycle exists
                                        Just winIdx -> return (pl !! winIdx)
                                    -- an additional reorder action is taken place here, where the players are ordered as 
    where
        pn = length pl
        eboard = eraseBoard (playerColourList pn) externalBoard
        iboards = replicate pn startBase
        hts = replicate pn RBLeaf
        kms = replicate pn (replicate pn [])

-- run a game with certain setting several times and return lists of winner players and the playouts taken from the game
multipleRuns :: Int -> MCTSControl -> [Player] -> IO [Player]
multipleRuns runs control pl = let pls = replicate runs pl
                               in  do mapM (runFromInitialState control) pls

-- run several sets with different player settings and each sets contain multiple runs
multipleGames :: Int -> MCTSControl -> [[Player]] -> IO [Player]
multipleGames runs control pls = do results <- mapConcurrently (multipleRuns runs control) pls
                                    return $ concat results

-- write the input to a certain file of given filename
experimentRecord :: [Player] -> FilePath -> IO ()
experimentRecord ws fileName = do path2 <- openFile winnersFile WriteMode
                                  hPutStr path2 (show ws)
                                  hClose path2
                                  return ()
    where
        winnersFile = fileName ++ "_winners.txt"

-- ghc -main-is Experiment Experiment.hs -O2 -threaded -outputdir dist
-- this is just for testing purpose, run game several times with the fixed setting
main :: IO ()
main = do arg <- getArgs
          start <- lookupTable `seq` getCurrentTime

          {-let runs = read $ head arg :: Int
              player = read $ arg !! 1 :: Player
          result <- runMultipleSimulations runs player-}
          let
              -- time: from 0.05s to 0.5s, and finally 5s
              time = read $ head arg :: Double 
              runs = read $ arg !! 1 :: Int                               
              idx  = read $ arg !! 2 :: Int
              str = printf "%.3f" time
              fileName = "./experiments/test5/" ++ str ++ "_" ++ show idx
              testSet  = generatePlayerList 3 [(Move, 0), (Board, 0), (PParanoid, 4), (PBRS, 4)] -- 60 combinations, each assignment runs 30 times
              control = (Nothing, Just time)
              
        -- test0 stores the time cost for 1000 playouts
        -- test1 stores the (three-player) experimtal trials of [(Move, 0), (Board, 0), (MParanoid, 2), (MBRS, 2)]
        -- test2 stores the set of [(Move, 0), (Board, 0), (OParanoid, 2), (OBRS, 2)]
        -- test3 stores the set of [(Move, 0), (Board, 0), (PParanoid, 2), (PBRS, 2)]
        -- test4 and test5 stores the percentage-based minimax search as well but with search depth of 3 and 4

          result <- multipleGames runs control (divide2Chunks 6 testSet idx)
          experimentRecord result fileName
          end <- result `seq` getCurrentTime
          putStrLn $ "Time cost: " ++ show (diffUTCTime end start)
          -- putStrLn $ show player ++ "'s time cost: " ++ show result ++ "s"
          putStrLn "Completed"

-- a list of tested player settings
-- need to be adjusted when running experiments
-- standardOrder :: [Player]
-- standardOrder = [(Random, 0), (Move, 0), (Board, 0), (PParanoid, 3), (PBRS, 3)]

-- divide the player arrangements into several smaller sets and pick one of them
divide2Chunks :: Int -> [a] -> Int -> [a]
divide2Chunks chunks ps idx = chunksOf chunkSize ps !! idx
    where
        chunkSize = length ps `div` chunks

-- loadWinner :: Int -> MCTSControl -> [Int] -> IO [Player]
-- loadWinRates folderIndex control indices = do winners <- loadWinners folderIndex control indices 
--                                               return (concat winners)
                                              -- return $ calculateWinRate standardOrder (concat winners)
    -- where
    --     calculateWinRate :: [Player] -> [Player] -> [Double]
    --     calculateWinRate [] _ = []
    --     calculateWinRate (p:ps) xs = fromIntegral (length (p `elemIndices` xs)) / fromIntegral (length xs) : calculateWinRate ps xs

{-
loadAveragePlayouts :: Int -> MCTSControl -> [Int] -> IO [Double]
loadAveragePlayouts folderIndex control indices = do playouts <- loadPlayouts folderIndex control indices
                                                     let combinedList = transpose playouts
                                                     return (map mean combinedList)

loadPlayouts :: Int -> MCTSControl -> [Int] -> IO [[[Int]]]
loadPlayouts _ _ [] = return []
loadPlayouts folderIndex control@(iterations, time) (i:is) = 
                                       do playouts <- loadExperimentData fileName :: IO [[Int]]
                                          rest <- loadPlayouts folderIndex control is
                                          return (playouts:rest)
    where
        str = if isNothing iterations then printf "%.3f" $ fromMaybe 0 time
              else show $ fromMaybe 0 iterations
        
        fileName = "./experiments/test" ++ show folderIndex ++ "/" ++ str ++ "_" ++ show i  ++ "_playouts.txt"
-}

-- if wanting to calculate the win rates of across all assignments, the result might not be too detailed
-- in order to discover with more depth, the assignments are evaluated in two groups, one is the tournament assignment where only two players are involved
-- and another one is multi-player group where more than two players played the game, in a three-player game, this mean three different players
-- in this way, it tests the performance of players against each other as well as the performance of completing againt different players at the same time

getSubset1 :: Player -> Player -> Double -> [Player] -> Int -> IO ()
getSubset1 p1 p2 time ps folderIndex = 
                     do ws <- getWinners folderIndex (Nothing, Just time) [0..5]
                        let sections = chunksOf 30 ws
                            twoPlayers = tournamentList 3 p1 p2
                            allPlayers = generatePlayerList 3 ps
                            positions = getIndices twoPlayers allPlayers
                            winners = concatMap (sections !!) positions
                        print (show p1 ++ ": " ++ show (winRate p1 winners))
                        print (show p2 ++ ": " ++ show (winRate p2 winners))

getIndices :: Eq a => [a] -> [a] -> [Int]
getIndices [] _ = []
getIndices (x:xs) ps = case x `elemIndex` ps of
                        Nothing -> error "Not exist item"
                        Just idx -> idx:getIndices xs ps

winRate :: (Fractional a1, Eq a2) => a2 -> [a2] -> a1
winRate x xs = fromIntegral (length (x `elemIndices` xs)) / fromIntegral (length xs)

getSubset2 :: Player -> Double -> [Player] -> Int -> IO ()
getSubset2 p time ps folderIndex= 
                 do ws <- getWinners folderIndex (Nothing, Just time) [0..5]
                    let sections = chunksOf 30 ws
                        allPlayers = generatePlayerList 3 ps
                        threePlayers = filter (p `elem`) (filter notSamePlayers allPlayers)
                        positions = getIndices threePlayers allPlayers
                        winners = concatMap (sections !!) positions
                    print (show p ++ ": " ++ show (winRate p winners))
    where
        notSamePlayers xs = length (nub xs) == length xs

getWinners :: Int -> MCTSControl -> [Int] -> IO [Player]
getWinners folderIndex control is = do winnerList <- loadWinners folderIndex control is
                                       return $ concat winnerList
    where
        loadWinners :: Int -> MCTSControl -> [Int] -> IO [[Player]]
        loadWinners _ _ [] = return []
        loadWinners folderIndex control@(iterations, time) (i:is) = do winners <- loadExperimentData fileName :: IO [[Player]]
                                                                       rest <- loadWinners folderIndex control is
                                                                       return (winners ++ rest)
            where
                str = if isNothing iterations then printf "%.3f" $ fromMaybe 0 time else show $ fromMaybe 0 iterations
                fileName = "./experiments/test" ++ show folderIndex ++ "/" ++ str ++ "_" ++ show i  ++ "_winners.txt"


loadExperimentData :: Read b => FilePath -> IO [b]
loadExperimentData fileName = do filePath <- openFile fileName ReadMode
                                 contents <- hGetContents filePath
                                 return $ convertToElement $ lines contents
    where
        convertToElement s = map read s



