module Experiment where
-- setup a draft experiment set for different AI players
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

-- retrieve the average value of a list
mean xs = realToFrac (sum xs) / genericLength xs

-- retrieve the median value of a list (regardless of the amount of certain value)
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

-- get the turn where the algorithm that converges to 0 turn
convergeTurn :: [Int] -> Int
convergeTurn [] = 0 -- when all turns are 0
convergeTurn xs = let x = last xs
                  in  if x /= 0 then length xs -- when the input length is excatly equal to the output, meaning that the convergence is not reached
                      else convergeTurn (init xs)

-- random move player: just randomly choose a move regradless of the benefit
randomMoveDecision :: GameTreeStatus -> Board
randomMoveDecision s@(pi, _, board, pn, _, _)= let co = playerColour pi pn
                                                   bs = evalState (colouredMovesList co) s
                                                   ns = expandPolicy co bs -- the backward movement should be restricted, otherwise, no progress might be made
                                                   rm = bs !! randomMove (length bs) -- randomly choose one expanded result
                                               in  evalState (repaintBoard rm) s -- return the chosen board state                                  
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Experiment 1
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
multipleCalls pn board cons iter = let r = convergeTurn (singleCall pn board cons)
                                       rs = multipleCalls pn board cons (iter - 1)
                                   in  r `par` rs `pseq` r:rs
-- given a board state, call the MCTS function for deciding the next move, and return the turns of game simulatons taken in the playout phase
singleCall :: Int -> Board -> (Double, Double) -> [Int]
singleCall pn board cons = let (root, rootIdx) = makeRoot pn board
                               (_, _, _, pl) = finalSelection root (0, rootIdx, board, pn, RBLeaf, cons) 0 500
                           in  pl
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Experiment 2
-- matching with the random players with different parameters of selection strategy
experiment2 :: Int -> Board -> [(Double, Double)] -> [[Int]] -> Int -> [[(Int, Int)]]
experiment2 _ _ [] _ _ = []
experiment2 pn board (a:as) is iter = let r = differentOrderGames (0, 0, board, pn, RBLeaf, a) is 1 iter
                                          rs = experiment2 pn board as is iter
                                      in  r `par` rs `pseq` r:rs
-- repeat the game with the same selecting strategy but different player order and amount
differentOrderGames :: GameTreeStatus -> [[Int]] -> Int -> Int -> [(Int, Int)]
differentOrderGames _ [] _ _ = []
differentOrderGames s (i:is) gs iter = let r = multipleGames s i gs iter
                                           rs = differentOrderGames s is gs iter
                                       in  r `par` rs `pseq` r ++ rs
-- repeat the game with the same setting several times
multipleGames :: GameTreeStatus -> [Int] -> Int -> Int -> [(Int, Int)]
multipleGames _ _ _ 0 = []
multipleGames s mctsPi gs iter = let r = singleGame s mctsPi gs
                                     rs = multipleGames s mctsPi gs (iter - 1)
                                 in  r `par` rs `pseq` r:rs
-- given a certain setting, and hold a game againt the random-choice player
-- return the winning player, game turns
singleGame :: GameTreeStatus -> [Int] -> Int -> (Int, Int)
singleGame s@(pi, bi, board, pn, ht, cons) mctsPi gs = let colour = playerColour pi pn
                                                       in  if pi `notElem` mctsPi then let newBoard = randomMoveDecision s
                                                                                           winIdx = checkPlayersWinState pn newBoard
                                                                                       in  if winIdx /= -1 then if winIdx == pi then (0, getTurns gs pn) -- 0 if MCTS loses
                                                                                                                else (1, getTurns gs pn) -- 1 if MCTS wins
                                                                                           else singleGame (turnBase pn pi, bi, newBoard, pn, ht, cons) mctsPi (gs+1)
                                                           else let (root, rootIdx) = makeRoot pn board
                                                                    (newBoard, _, nht, _) = finalSelection root (pi, rootIdx, board, pn, ht, cons) 0 10
                                                                    winIdx = checkPlayersWinState pn newBoard
                                                                in  if winIdx /= -1 then if winIdx `elem` mctsPi then (1, getTurns gs pn) -- check win state after a movement is made
                                                                                         else (0, getTurns gs pn)
                                                                    else singleGame (turnBase pn pi, bi, newBoard, pn, nht, cons) mctsPi (gs+1)

-- ghc -main-is Experiment Experiment.hs -O2 -fllvm -outputdir dist
-- main = do arg <- getArgs
--           start <- getCurrentTime
--           let pn = read (head arg) -- the player number
--               iter = read (arg !! 1) -- the iteration of a single call being repeated
--               xs -- construct the initial board state and run the experiment with that board
--                 | pn == 2 = experiment1 2 (eraseBoard twoPlayersSet) settings1 iter
--                 | pn == 3 = experiment1 3 (eraseBoard threePlayersSet) settings1 iter
--                 | pn == 4 = experiment1 4 (eraseBoard fourPlayersSet) settings1 iter
--                 | pn == 6 = experiment1 6 externalBoard settings1 iter
--                 | otherwise = []
--               fn = "experiment1_" ++ show pn ++ ".txt"
--           experimentRecord xs fn -- record the collected results into a file
--           end <- getCurrentTime
--           print $ diffUTCTime end start -- display the used time of the experiment

main = do arg <- getArgs
          start <- getCurrentTime
          let pn = read (head arg) -- the player number
              bo -- construct the initial board state
                | pn == 2 = eraseBoard twoPlayersSet
                | pn == 3 = eraseBoard threePlayersSet
                | pn == 4 = eraseBoard fourPlayersSet
                | pn == 6 = externalBoard
                | otherwise = []
              it = read (arg !! 1)
              is = read (arg !! 2)
              xs = experiment2 pn bo settings1 is {-(settings2 pn 2)-} it
              fn = "experiment2_" ++ show pn ++ "_" ++ show is ++ ".txt"
          experimentRecord xs fn
          end <- getCurrentTime
          print $ diffUTCTime end start -- display the used time of the experiment

-- systematic test for optimising the parameters for game tree evaluation based on playout convergence
settings1 :: [(Double, Double)]
settings1 = [(x, y) | x <- map fromRational [0 .. 5], y <- map fromRational [0 .. 5]]

-- systematic test for optimising the parameters for game tree evaluation based on matches information
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

-- write the input to a certain file of given filename
experimentRecord :: Show a => [a] -> FilePath -> IO ()
experimentRecord xs filePath = do filePath <- openFile filePath WriteMode
                                  hPutStr filePath (convertToString xs)
                                  hClose filePath
                                  return ()
    where
        convertToString [] = ""
        convertToString (x:xs) = show x ++ "\n" ++ convertToString xs
