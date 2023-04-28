module Experiment where
-- setup experimental trials for testing MCTS

import MCTS ( getTurns, playout )
import GameTree
    ( turnBase,
      GameTree(GRoot),
      HistoryTrace,
      KillerMoves,
      PlayerIndex,
      PlayoutEvaluator (..),
      GameTreeStatus, getBoardIndex, BoardIndex, PlayoutArgument, getTransform, turn2Root, getChildren)
import Board
    ( eraseBoard,
      externalBoard,
      playerColourList,
      printEoard,
      replace,
      startBase,
      Board,
      Pos, removeByIdx, goalBase, Transform )
import RBTree ( RBTree(RBLeaf) )
import Zobrist ( winStateDetect )
import System.Environment (getArgs)
import System.Random (newStdGen)
import Extension (finalSelection, MCTSControl)
import Configuration (lookupTable)
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
import Control.Concurrent.Async ( mapConcurrently, mapConcurrently_ )
import Data.Maybe (fromMaybe, isNothing, isJust)

{-
-- based on the valid assignments given, only select the ones with two players involved
twoPlayerList :: Int -> PlayoutArgument -> PlayoutArgument -> [[PlayoutArgument]]
twoPlayerList pn p1 p2 = filter (not . samePlayers) (binaryPlayerArrangement pn)
    where
        -- a list of combinations of two players
        binaryPlayerArrangement 0 = [[]]
        binaryPlayerArrangement pn = [x:xs | x <- [p1, p2], xs <- binaryPlayerArrangement (pn - 1)]

        -- check if all players are the same in the generated arrangements
        samePlayers [] = True
        samePlayers [_] = True
        samePlayers (x:y:zs) = x == y && samePlayers (y:zs)
-}

multiPlayerList :: Int -> [PlayoutArgument] -> [[PlayoutArgument]]
multiPlayerList pn ps = let pl = multiPlayerArrangement pn [0 .. length ps - 1]
                        in  map (findItemByIdx ps) pl
    where
        multiPlayerArrangement :: Int -> [Int] -> [[Int]]
        multiPlayerArrangement 0 _ = [[]]
        multiPlayerArrangement pn ps = [x:xs | x <- ps, xs <- multiPlayerArrangement (pn-1) (removeItem x ps)]

        removeItem :: Int -> [Int] -> [Int]
        removeItem _ [] = []
        removeItem y (x:xs) = if y == x then xs else x:removeItem y xs

        findItemByIdx :: [a] -> [Int] -> [a]
        findItemByIdx _ [] = []
        findItemByIdx ps (i:is) = ps !! i:findItemByIdx ps is

--Run Experimental Trials---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- given a player, run the playout phase from the initial board till the end of the game
-- this is applied to see the search speed of each evaluator 
{-
runSimulation :: PlayoutArgument -> IO Double
runSimulation player = do start <- getCurrentTime
                          newGen <- newStdGen
                          -- the position of the player is not important here, so it is fixed to 0
                          let winIdx = start `seq` evalState (playout 0) (newGen, 0, 1, eboard, iboards, pn, RBLeaf, (0.5, 5), player)
                          end <- winIdx `seq` getCurrentTime
                          return $ realToFrac $ end `seq` diffUTCTime end start

    where
        pn = 3  
        eboard = eraseBoard (playerColourList pn) externalBoard
        iboards = replicate pn startBase

-- run the simulation several times from the initial board state
runMultipleSimulations :: Int -> PlayoutArgument -> IO Double
runMultipleSimulations runs player = let pls = replicate runs player
                                     in  do durations <- mapM runSimulation pls
                                            return $ sum durations
-}

-- given a certain status, and run a game with different players till one of the players wins, and return the data for evaluating the performance
-- besides, one thing to point out is that the history trace is maintained by each player specifically
singleRun :: MCTSControl -> PlayerIndex -> Board -> [[Pos]] -> Int -> [HistoryTrace] ->
             [PlayoutArgument] -> [Board] -> (Double, Double) -> IO (Maybe PlayerIndex)
singleRun control pi eboard iboards pn hts pl record cons =
                                    -- there might exist cycling where all players are trying to maintain a board state that is most benefical for them
                                    -- therefore, it is necessary to have a mechanism to break the loop 
                                    if checkLoop eboard record then return Nothing
                                    else do gen <- newStdGen
                                            (neboard, niboard, nht) <- finalSelection (GRoot 0 []) (gen, pi, 1, eboard, iboards, pn, hts !! pi, cons, pl !! pi) control
                                            system "cls"
                                            printEoard neboard                   
                                            if winStateDetect niboard then return $ Just pi
                                            else let niboards = replace pi niboard iboards
                                                     nextTurn = turnBase pn pi
                                                     newHistory = replace pi nht hts
                                                 in  niboards `par` nextTurn `pseq`
                                                     singleRun control nextTurn neboard niboards pn newHistory pl (neboard:record) cons
                                                     -- in this way, the avaliable subtree is inherited for each player
    where
        -- if the current game turn exceeds 150 as well as existing several repeating board states, then this is defined as a loop/cycle
        checkLoop input boardList = getTurns (length record) pn >= 150 && length (input `elemIndices` boardList) >= 5

-- start the game from the initial board state
runFromInitialState :: (Double, Double) -> MCTSControl -> [PlayoutArgument] -> IO PlayoutArgument
runFromInitialState cons control pl = do result <- singleRun control 0 eboard iboards pn hts pl [] cons
                                         case result of
                                            Nothing -> runFromInitialState cons control pl -- rerun the game if cycle exists
                                            Just winIdx -> return (pl !! winIdx)
    where
        pn = length pl
        eboard = eraseBoard (playerColourList pn) externalBoard
        iboards = replicate pn startBase
        hts = replicate pn RBLeaf

-- run a game with certain setting several times and return lists of winner players
multipleRuns :: (Double, Double) -> Int -> MCTSControl -> [PlayoutArgument] -> IO [PlayoutArgument]
multipleRuns cons runs control pl = let pls = replicate runs pl
                                    in  do mapConcurrently (runFromInitialState cons control) pls

-- run several sets with different player settings and each sets contain multiple runs
multipleGames :: (Double, Double) -> Int -> MCTSControl -> [[PlayoutArgument]] -> IO [PlayoutArgument]
multipleGames cons runs control pls = do results <- mapM (multipleRuns cons runs control) pls
                                         return $ concat results

-- write the input to a certain file of given filename
experimentRecord :: Show a => a -> FilePath -> IO ()
experimentRecord ws fileName = do path <- openFile fileName WriteMode
                                  hPutStr path (show ws)
                                  hClose path
                                  return ()
{-
testConstantPairs :: PlayoutArgument -> [Double] -> [Double] -> IO ()
testConstantPairs player cs ws = do mapM_ (testConstantPair player cs) ws

testConstantPair :: PlayoutArgument -> [Double] -> Double -> IO ()
testConstantPair player cs w = do mapM_ (testConstant player w) cs

testConstant :: PlayoutArgument -> Double -> Double -> IO () 
testConstant player w c = do results <- multipleGames (c, w) 20 (Nothing, Just 0.5) (twoPlayerList 3 defaultPlayer player)
                             experimentRecord results ("./experiments0/" ++ show (player, defaultPlayer) ++ "(" ++ printf "%.2f" c ++ "," ++ printf "%.2f" w ++ ").txt")
    where
        defaultPlayer = (Random,0,0)

main = do arg <- getArgs
          let p1 = read $ head arg :: PlayoutArgument
              ws = read $ arg !! 1
              cs = [0.1, 0.2, 0.3, 0.4, 0.5]
          testConstantPairs p1 cs ws
-}

{-
main = do arg <- getArgs
          let control = read $ arg !! 0
              ps = read $ arg !! 1
          result <- runFromInitialState (0.1, 5.0) control ps
          print result
          return ()
-}

-- ghc -main-is Experiment Experiment.hs -O2 -threaded -outputdir dist
main :: IO ()
main = do arg <- getArgs
          let input = read $ arg !! 0 :: Double
              player1 = read $ arg !! 1 :: PlayoutArgument
              -- player2 = read $ arg !! 2 :: PlayoutArgument
              pairs = [ (Move,0,0), (Random,0,0), player1
                    --    ((Move, 0, 0), (Board, 0, 0)),
                    --    ((Move, 0, 0), (Random, 0, 0)),
                    --    ((Board, 0, 0), (Random, 0, 0))
                    --    ((Random,0,0),player1),
                    --    ((Move,0,0),player1),
                    --    ((Board,0,0),player1)
                    --    (player1, player2)
                      ]
          result <- autoRunExperiment 167 (0.1, 5) input pairs
          {-
          let runs = read $ head arg :: Int
              player = read $ arg !! 1 :: PlayoutArgument
          result <- runMultipleSimulations runs player
          putStrLn $ show player ++ "'s time cost: " ++ show result ++ "s"
          -}
          result `seq` putStrLn "All Completed!"
          return ()


--Arrange Test Sets---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- the test set of several players playing against each other
autoRunExperiment :: Int -> (Double, Double) -> Double -> [PlayoutArgument] -> IO ()
autoRunExperiment runs cons input ps = let str = printf "%.2f" input
                                           -- str = "(" ++ printf "%.2f" (fst cons) ++ "," ++ printf "%.2f" (snd cons) ++ ")"
                                           folderName = "./experiments/"
                                           fileName = folderName ++ show ps ++ "_" ++ str ++ ".txt"
                                           -- create the three-player game of two player types
                                           testSet = {-twoPlayerList 3 p1 p2-} multiPlayerList 3 ps
                                       in  do start <- getCurrentTime
                                              result <- multipleGames cons runs (Nothing, Just input) testSet
                                              -- record the winners
                                              experimentRecord result fileName
                                              end <- getCurrentTime
                                              putStrLn $ "Time cost: " ++ show (diffUTCTime end start)
                                              {-putStrLn $ show p1 ++ ": " ++ show (winRate p1 result)
                                              putStrLn $ show p2 ++ ": " ++ show (winRate p2 result)-}


-- calculate the win rate of certain player in a list of winners
winRate :: PlayoutArgument -> [PlayoutArgument] -> Double
winRate x xs = fromIntegral (length (x `elemIndices` xs)) / fromIntegral (length xs)

{-
-- get the win rate of between two players in a three-player game
getWinRate :: PlayoutArgument -> PlayoutArgument -> Double -> IO ()
getWinRate p1 p2 input = do winners <- loadExperimentData fileName :: IO [PlayoutArgument]
                            putStrLn (show p1 ++ ": " ++ printf "%.3f" (winRate p1 winners))
                            putStrLn (show p2 ++ ": " ++ printf "%.3f" (winRate p2 winners))
    where
        fileName = "./experiments/" ++ show (p1, p2) ++ "_" ++ printf "%.2f" input ++ ".txt"
        -- fileName = "./experiments0/" ++ show (p1, p2) ++ "(" ++ printf "%.2f" c ++ "," ++ printf "%.2f" w ++ ").txt"
-}

-- read the content from the file
loadExperimentData :: Read b => FilePath -> IO [b]
loadExperimentData fileName = do filePath <- openFile fileName ReadMode
                                 contents <- hGetContents filePath
                                 return $ read contents
