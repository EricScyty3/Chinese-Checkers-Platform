module Experiment where
-- setup experimental trials for testing MCTS with different strategies

import MCTS
import GameTree
import Board
import Control.Monad.State
import RBTree
import Data.Ratio
import Control.Parallel
import Data.Time
import Zobrist
import System.Environment (getArgs)
import Data.List
import System.IO
import GHC.IO

{-
-- retrieve the average value of a list
mean xs = realToFrac (sum xs) / genericLength xs

-- retrieve the median value of a list (regardless of the amount of certain value)
getMedianValue :: [Int] -> Double
getMedianValue xs = medianValue $ sort xs
    where
        medianValue :: [Int] -> Double
        medianValue [] = 0
        medianValue [x] = fromIntegral x
        medianValue [x, y] = fromIntegral (x+y) / 2
        medianValue xs = let minIdx = elemIndices (minimum xs) xs
                             maxIdx = elemIndices (maximum xs) xs
                             takeList = take (head maxIdx) xs
                             dropList = drop (last minIdx + 1) takeList
                         in  if null dropList then medianValue [minimum xs, maximum xs]
                             else medianValue dropList

median :: [Int] -> Double
median xs = let s = sort xs
            in  if odd (length s) then fromIntegral (s !! (length s `div` 2))
                else fromIntegral (s !! (length s `div` 2) + s !! ((length s `div` 2) - 1)) / 2

-- get the turn where the algorithm that converges to 0 turn
convergeTurn :: [Int] -> Int
convergeTurn [] = 0 -- when all turns are 0
convergeTurn xs = let x = last xs
                  in  if x /= 0 then length xs -- when the input length is excatly equal to the output, meaning that the convergence is not reached
                      else convergeTurn (init xs)

-- write the input to a certain file of given filename
experimentRecord :: Show a => [a] -> FilePath -> IO ()
experimentRecord xs filePath = do filePath <- openFile filePath WriteMode
                                  hPutStr filePath (convertToString xs)
                                  hClose filePath
                                  return ()
    where
        convertToString [] = ""
        convertToString (x:xs) = show x ++ "\n" ++ convertToString xs
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Experiment 0
-- run for one turn for different player on board, give an initial impression
{-
main = do arg <- getArgs
          start <- getCurrentTime
          let iter = read $ head arg
              cons = read (arg !! 1)
              x2 = singleCall 2 (eraseBoard (playerColourList 2)) cons iter
              x3 = singleCall 3 (eraseBoard (playerColourList 3)) cons iter
              x4 = singleCall 4 (eraseBoard (playerColourList 4)) cons iter
              x6 = singleCall 6 (eraseBoard (playerColourList 6)) cons iter
              fn = "./experiments/experiment0/experiment.txt"
              tx = (x2 `par` x3 `pseq` x2 : [x3]) ++ (x4 `par` x6 `pseq` x4 : [x6])
          experimentRecord tx fn
          end <- getCurrentTime
          print $ diffUTCTime end start
-}
-- load the data collected from the experiment 0
loadExperiment0Record :: [[Int]]
loadExperiment0Record = let filename = "./experiments/experiment0/experiment.txt"
                        in  unsafePerformIO $ do filePath <- openFile filename ReadMode
                                                 contents <- hGetContents filePath
                                                 return $ map read (lines contents)

-- pre-compute the raw data, calculating the average value, median value, and the turns of convergence
analyseData :: [[Int]] -> [[Double]]
analyseData [] = []
analyseData (x:xs) = let a = mean (filter (/=0) x)
                         b = getMedianValue x -- here applies value-sensitive median becuase too many zeros might cover the median value
                         c = fromIntegral $ convergeTurn x
                     in  [a, b, c]:analyseData xs

-- given a board state, call the MCTS function for deciding the next move, and return the turns of game simulatons taken in the playout phase
singleCall :: Int -> Board -> (Double, Double) -> Int -> [Int]
singleCall pn board cons iter = let (root, rootIdx) = makeRoot pn board
                                    (_, _, _, pl) = finalSelection root (0, rootIdx, board, pn, RBLeaf, cons) 0 iter
                                in  pl
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Experiment 1   
-- the one that first targets the range of the parameters from 0 to 5
{-
main = do arg <- getArgs
          start <- getCurrentTime
          let pn = read (head arg) -- the player number
              iter = read (arg !! 1) -- the iteration of a single call being repeated
              -- construct the initial board state and run the experiment with that board
              xs = experiment1 pn (eraseBoard (playerColourList pn)) settings1 iter
              fn = "./experiments/experiment1/experiment_" ++ show pn ++ ".txt"
          experimentRecord xs fn -- record the collected results into a file
          end <- getCurrentTime
          print $ diffUTCTime end start -- display the used time of the experiment
-}
-- after that, try to discover a more specific range by introducing the range with double numbers
{-
main = do arg <- getArgs
          start <- getCurrentTime
          let pn = read (head arg) -- the player number
              uct = read (arg !! 1) -- the uct constant could be remained integer
              iter = read (arg !! 2) -- the iteration of a single call being repeated
              xs = experiment1 pn (eraseBoard (playerColourList pn)) (settings1_2 uct) iter
              fn = "experiments/experiment1/experiment_" ++ show pn ++ "_" ++ show uct ++ ".txt"
          experimentRecord xs fn -- record the collected results into a file
          end <- getCurrentTime
          print $ diffUTCTime end start -- display the used time of the experiment     
-}
-- list all potential parameter pairs for the combination of UCT and Progressive History
settings1 :: [(Double, Double)]
settings1 = [(x, y) | x <- map fromRational [0 .. 5], y <- map fromRational [0 .. 5]]
-- the parameter pairs with the first one fixed and the second one various
settings1_2 :: Int -> [(Double, Double)]
settings1_2 x = [(fromIntegral x, y) | y <- map fromRational [0.1, 0.2 .. 1]]

-- load the data collected from the experiment 1, with a rough range of parameters
loadExperiment1Record :: Int -> [((Double, Double), Double)]
loadExperiment1Record pn = let filename = "./experiments/experiment1/experiment_" ++ show pn ++ ".txt"
                               ps = unsafePerformIO $ do filePath <- openFile filename ReadMode
                                                         contents <- hGetContents filePath
                                                         return $ map read (lines contents)
                          in  filter ((/=500) . snd) (zip settings1 (map mean ps))
                          -- since the iterations of singleCall is 500, any value above or close to this means it fails to converge before 500 turns

-- load the data collected from the part 2 of experiment 1, with more detailed parameters
loadExperiment1_2Record :: Int -> Int -> [((Double, Double), Double)]
loadExperiment1_2Record pn uct = let filename = "./experiments/experiment1/experiment_" ++ show pn ++ "_" ++ show uct ++ ".txt"
                                     ps = unsafePerformIO $ do filePath <- openFile filename ReadMode
                                                               contents <- hGetContents filePath
                                                               return $ map read (lines contents)
                                in  zip (settings1_2 uct) (map mean ps) -- similar computation

-- given a set of agruments, collect how the playouts converge with the selection strategy of different parameters
experiment1 :: Int -> Board -> [(Double, Double)] -> Int -> [[Int]]
experiment1 _ _ [] _ = []
experiment1 pn board (a:as) iter = let t = multipleCalls pn board a iter
                                       rs = experiment1 pn board as iter
                                   in  t `par` rs `pseq` t:rs
-- repeat calling the MCTS function several times such that is could be statistically meaningful 
-- the less it is, the faster it converges during the playouts
multipleCalls :: Int -> Board -> (Double, Double) -> Int -> [Int]
multipleCalls _ _ _ 0 = []
multipleCalls pn board cons iter = let r = convergeTurn (singleCall pn board cons 500) -- default iterations for singleCall is 500
                                       rs = multipleCalls pn board cons (iter - 1)
                                   in  r `par` rs `pseq` r:rs
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Experiment 2
-- simulation several game mathcing against the mcts and random player of different turn order
{-
main = do arg <- getArgs
          start <- getCurrentTime
          let pn = 3 -- the board is of 3 players
              uct = read (head arg) -- the uct constant
              bo = eraseBoard (playerColourList pn) -- initial board
              it = read (arg !! 1) -- trials for single game
              xs = experiment2 pn bo (settings1_2 uct) (settings2 pn 2) it
              fn = "experiments/experiment2/experiment2_" ++ show uct ++ ".txt"
          experimentRecord xs fn
          end <- getCurrentTime
          print $ diffUTCTime end start -- display the used time of the experiment
-}
-- the order of players given the invovled players amount, 
-- for instance, in a 3-player game or 2 players types: random and mcts, there are 6 assignments where the mcts players could loacted at: [2],[1],[1,2],[0],[0,2],[0,1]
settings2 :: Int -> Int -> [[Int]]
settings2 l t = let ps = permutation l t
                    ns = filter (not . sameElements) ps -- removing the arrangements of the same players
                in  map (elemIndices 1) ns -- transform into indices
    where
        sameElements xs = let is = elemIndices (head xs) xs
                          in  length is == length xs
-- arrangements of players in several types of certain length
permutation :: Int -> Int -> [[Int]]
permutation 0 _ = [[]]
permutation l t = [x:xs | x <- [0 .. (t-1)], xs <- permutation (l - 1) t]

-- matching with the random players with different parameters of selection strategy
experiment2 :: Int -> Board -> [(Double, Double)] -> [[Int]] -> Int -> [[(Int, Int)]]
experiment2 _ _ [] _ _ = []
experiment2 pn board (a:as) pi iter = let r = experiment2' pn board a pi iter
                                          rs = experiment2 pn board as pi iter
                                      in  r `par` rs `pseq` r:rs
    where
        -- start the game with mcts player of different orders
        experiment2' :: Int -> Board -> (Double, Double) -> [[Int]] -> Int -> [(Int, Int)]
        experiment2' _ _ _ [] _ = []
        experiment2' pn board cons (i:is) iter = let r = multipleGames board pn cons i iter 
                                                     rs = experiment2' pn board cons is iter
                                                 in  r `par` rs `pseq` r ++ rs

-- repeat the game with the same setting several times
multipleGames :: Board -> Int -> (Double, Double) -> [Int] -> Int -> [(Int, Int)]
multipleGames _ _ _ _ 0 = []
multipleGames board pn cons mctsPi iter = let r = singleGame 0 board pn RBLeaf cons mctsPi 0
                                              rs = multipleGames board pn cons mctsPi (iter - 1)
                                          in  r `par` rs `pseq` r:rs

-- random move player: just randomly choose a move regradless of the benefit
randomMoveDecision :: PlayerIndex -> Board -> Int -> Board
randomMoveDecision pi board pn= let co = playerColour pi pn
                                    bs = evalState (colouredMovesList co) (pi, 0, board, pn, RBLeaf, (0, 0)) -- list all possible resulting boards
                                    ns = expandPolicy co bs -- the backward movements should be restricted, otherwise, no progress might be made
                                    rm = bs !! randomMove (length bs) -- randomly choose one of the expanded results
                                in  evalState (repaintBoard rm) (pi, 0, board, pn, RBLeaf, (0, 0)) -- return the chosen board state 

-- given a certain status, and hold a game againt the random-choice player
-- return the winner, turns taken to end the game
singleGame :: PlayerIndex -> Board -> Int -> HistoryTrace -> (Double, Double) -> [Int] -> Int -> (Int, Int)
singleGame pi board pn ht cons mctsIdx ms = case pi `notElem` mctsIdx of -- if the current turn is for the random player
                                                True -> let newBoard = randomMoveDecision pi board pn -- perform random play
                                                            winIdx = checkPlayersWinState pn newBoard -- detect the win state
                                                        in  if winIdx /= -1 then if winIdx == pi then (0, getTurns ms pn) -- random player wins, setup a flag of 0
                                                                                 else (1, getTurns ms pn) -- mcts player wins, setup a flag of 1
                                                            else singleGame (turnBase pn pi) newBoard pn ht cons mctsIdx (ms+1) -- if no win, then continue the game

                                                -- if the current turn is for the mcts player
                                                False -> let (root, rootIdx) = makeRoot pn board
                                                             (newBoard, _, nht, _) = finalSelection root (pi, rootIdx, board, pn, ht, cons) 0 10 -- setup mcts with 10 iterations
                                                             winIdx = checkPlayersWinState pn newBoard  -- check win state after a movement is made
                                                         in  if winIdx /= -1 then if winIdx `elem` mctsIdx then (1, getTurns ms pn) -- mcts player wins
                                                                                  else (0, getTurns ms pn) -- random player wins
                                                             else singleGame (turnBase pn pi) newBoard pn nht cons mctsIdx (ms+1) -- if no win, then continue the game

-- load the data from the experiment 2
-- loadExperiment2Record :: Int -> [((Double, Double), (Double, Double, Double))]
loadExperiment2Record uct = let filename = "./experiments/experiment2/experiment2_" ++ show uct ++ ".txt"
                                ps = unsafePerformIO $ do filePath <- openFile filename ReadMode
                                                          contents <- hGetContents filePath
                                                          return $ map read (lines contents) 
                                ws = map (map fst) ps :: [[Int]]
                                ts = map (map snd) ps :: [[Int]]
                                winRate = map mean ws
                                averageTurns = map mean ts
                                medianTurns = map median ts
                                xs = zip (settings1_2 uct) (zip3 winRate averageTurns medianTurns)
                            in  filter ((==1) . extractFirst . snd) xs -- removes the ones that fail reaching win rate of 100%
    where
        extractFirst :: (a, b, c) -> a
        extractFirst (a,_,_) = a -}