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
import Data.Maybe (fromMaybe, isNothing, isJust)


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

--Run Experimental Trials---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- given a player, run the playout phase from the initial board till the end of the game
-- this is applied to see the search speed of each evaluator 
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

-- given a certain status, and run a game with different players till one of the players wins, and return the data for evaluating the performance
-- besides, one thing to point out is that the history trace is maintained by each player specifically
singleRun :: MCTSControl -> PlayerIndex -> Board -> [[Pos]] -> Int -> [HistoryTrace] -> (Double, Double) ->
             [PlayoutArgument] -> [Board] -> [(GameTree, BoardIndex)] -> IO (Maybe PlayerIndex)
singleRun control pi eboard iboards pn hts cons pl record trees =
                                    -- there might exist cycling where all players are trying to maintain a board state that is most benefical for them
                                    -- therefore, it is necessary to have a mechanism to break the loop 
                                    if checkLoop eboard record then return Nothing
                                    else do gen <- newStdGen
                                            ((treeNode, newTreeIdx), neboard, niboard, nht) <- finalSelection currentTree (gen, pi, currentTreeIdx, eboard, iboards, pn, hts !! pi, cons, pl !! pi) control
                                            -- system "cls"
                                            -- printEoard neboard
                                            let newHistory = replace pi nht hts
                                                move = getTransform treeNode
                                                newTrees = renewSearchTrees move 0 $ replace pi (turn2Root treeNode, newTreeIdx) trees
                                            if winStateDetect niboard then return $ Just pi
                                            else let niboards = replace pi niboard iboards
                                                     nextTurn = turnBase pn pi
                                                 in  niboards `par` nextTurn `par` newTrees `pseq`
                                                     singleRun control nextTurn neboard niboards pn newHistory cons pl (neboard:record) newTrees
                                                     -- in this way, the avaliable subtree is inherited for each player
    where
        -- if the current game turn exceeds 150 as well as existing several repeating board states, then this is defined as a loop/cycle
        checkLoop input boardList = getTurns (length record) pn >= 150 && length (input `elemIndices` boardList) >= 5
        -- the current player's tree status
        (currentTree, currentTreeIdx) = trees !! pi
        
        renewSearchTrees :: Transform -> Int -> [(GameTree, BoardIndex)] -> [(GameTree, BoardIndex)]
        renewSearchTrees _ _ [] = []
        renewSearchTrees move idx (t:ts) = let newTree = if idx == pi then renewSearchTree move t else t
                                           in  newTree:renewSearchTrees move (idx+1) ts
                                            
        renewSearchTree :: Transform -> (GameTree, BoardIndex) -> (GameTree, BoardIndex)
        renewSearchTree move (tree, bi) = let children = getChildren tree
                                          in  case move `elemIndex` map getTransform children of
                                                Nothing -> (GRoot 0 [], 1) -- if not finding this branch, then the next turn will recreate a new tree
                                                Just idx -> (turn2Root $ children !! idx, bi) -- select the corresponding subtree


-- start the game from the initial board state
runFromInitialState :: (Double, Double) -> MCTSControl -> [PlayoutArgument] -> IO PlayoutArgument
runFromInitialState constant control pl = do result <- singleRun control 0 eboard iboards pn hts constant pl [] trees
                                             case result of
                                                Nothing -> runFromInitialState constant control pl -- rerun the game if cycle exists
                                                Just winIdx -> return (pl !! winIdx)
    where
        pn = length pl
        eboard = eraseBoard (playerColourList pn) externalBoard
        iboards = replicate pn startBase
        hts = replicate pn RBLeaf
        trees = replicate pn (GRoot 0 [], 1)

-- run a game with certain setting several times and return lists of winner players
multipleRuns :: (Double, Double) -> Int -> MCTSControl -> [PlayoutArgument] -> IO [PlayoutArgument]
multipleRuns constant runs control pl = let pls = replicate runs pl
                                        in  do mapConcurrently (runFromInitialState constant control) pls

-- run several sets with different player settings and each sets contain multiple runs
multipleGames :: (Double, Double) -> Int -> MCTSControl -> [[PlayoutArgument]] -> IO [PlayoutArgument]
multipleGames constant runs control pls = do results <- mapM (multipleRuns constant runs control) pls
                                             return $ concat results

-- write the input to a certain file of given filename
experimentRecord ws fileName = do path <- openFile fileName WriteMode
                                  hPutStr path (show ws)
                                  hClose path
                                  return ()

{-
testConstants c p = let ws = map fromRational [0.5, 0.6 .. 1] :: [Double]
                    in  do results <- repeatWithConstantW (fromIntegral c) ws p
                           experimentRecord results ("./experiments/" ++ show p ++ "_" ++ show c ++ ".txt")
    where
        repeatWithConstantW :: Double -> [Double] -> PlayoutArgument -> IO [Double]
        repeatWithConstantW _ [] _ = return []
        repeatWithConstantW constantC (w:ws) player = do result <- multipleGames (constantC, w) 100 (Just 10, Nothing) (twoPlayerList 3 (Move,0,0) player)
                                                         let winrate = wr player result
                                                         rest <- repeatWithConstantW constantC ws player
                                                         return $ winrate:rest
        
        wr x xs = fromIntegral (length (x `elemIndices` xs)) / fromIntegral (length xs)
-}

main :: IO ()
main = do arg <- getArgs
          let c = read $ head arg
              runs = read $ arg !! 1 
          results <- multipleGames c runs (Just 10, Nothing) (twoPlayerList 3 (Move,0,0) (Board,0,0))
          print $ winRate (Board,0,0) results


{-
-- ghc -main-is Experiment Experiment.hs -O2 -threaded -outputdir dist
main :: IO ()
main = do arg <- getArgs

          let input = read $ head arg
              runs = read $ arg !! 1 :: Int
              -- two players test
              
              depth = read $ arg !! 2 :: Int
              percentage = read $ arg !! 3 :: Int
              pairs = [
                       -- ((Move,0,0),(Board,0,0)),
                       ((Move,0,0),(MParanoid,depth,percentage)),
                       ((Move,0,0),(MBRS,depth,percentage)),
                       ((Board,0,0),(MParanoid,depth,percentage)),
                       ((Board,0,0),(MBRS,depth,percentage)),
                       ((MParanoid,depth,percentage),(MBRS,depth,percentage))
                      ]
                        
          result <- autoRunExperiments runs pairs input
          
          {-
          -- computation speed test
          let runs = read $ head arg :: Int
              player = read $ arg !! 1 :: PlayoutArgument
          result <- runMultipleSimulations runs player
          putStrLn $ show player ++ "'s time cost: " ++ show result ++ "s"
          -}
          result `seq` putStrLn "All Completed!"
-}

--Arrange Test Sets---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- the test set of several players playing against each other
{-
autoRunExperiments _ [] _ = return ()
autoRunExperiments runs ((p1,p2):ps) input =
                                       let str = printf "%.3f" input
                                           folderName = "./experiments/"
                                           fileName = folderName ++ show (p1, p2) ++ "_" ++ str ++ ".txt"
                                           -- create the three-player game of two player types
                                           testSet  = twoPlayerList 3 p1 p2
                                       in  do start <- getCurrentTime
                                              result <- multipleGames runs (Nothing, Just input) testSet
                                              -- record the winners
                                              experimentRecord result fileName
                                              end <- getCurrentTime
                                              putStrLn $ "Time cost: " ++ show (diffUTCTime end start) ++ ", " ++ show (p1, p2)
                                              putStrLn $ show p1 ++ ": " ++ winRate p1 result
                                              putStrLn $ show p2 ++ ": " ++ winRate p2 result
                                              -- keep running until all pairs are done
                                              autoRunExperiments runs ps input
-}
-- calculate the win rate of certain player in a list of winners
winRate :: PlayoutArgument -> [PlayoutArgument] -> String
winRate x xs = let wr = fromIntegral (length (x `elemIndices` xs)) / fromIntegral (length xs)
               in  printf "%.3f" (wr :: Double)

{-
-- get the win rate of between two players in a three-player game
getWinRate :: PlayoutArgument -> PlayoutArgument -> Double -> IO ()
getWinRate p1 p2 time =  do winners <- loadExperimentData fileName :: IO [PlayoutArgument]
                            putStrLn (show p1 ++ ": " ++ winRate p1 winners)
                            putStrLn (show p2 ++ ": " ++ winRate p2 winners)
    where
        str = printf "%.3f" time
        fileName = "./experiments/" ++ show (p1, p2) ++ "_" ++ str ++ ".txt"
-}

-- read the content from the file
loadExperimentData :: Read b => FilePath -> IO [b]
loadExperimentData fileName = do filePath <- openFile fileName ReadMode
                                 contents <- hGetContents filePath
                                 return $ read contents