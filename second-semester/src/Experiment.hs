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
      Pos )
import RBTree ( RBTree(RBLeaf) )
import Zobrist ( winStateDetect )
import System.Environment (getArgs)
import System.Random (newStdGen)
import Extension (finalSelection)
import Configuration (lookupTable)
import Data.Time (getCurrentTime)
import Data.Time.Clock (diffUTCTime)
import Control.Parallel (par, pseq)
import Data.Fixed (Pico)
import Data.List (elemIndices, sort)
import Minimax (MGameTreeStatus)
import Control.Monad.State (evalState)
import Data.List.Extra (chunksOf)
import System.IO ( hClose, openFile, hPutStr, IOMode(WriteMode) )
import System.Process (system)


-- during the experimental trials, several outcomes are measured under the contorl of iteration counts and time limits
-- the first is the total win rate in the multiple players game, either only two player types are invovled or the same number of the total players
-- the second is the (median) playouts, or more specific, the iterations that could be performed under the set time

-- a list of players' settings to be invovled in the experimental trials, containing the evaluator to be used during the playout phase 
-- as well as the search depth
type PlayerList = [(PlayoutEvaluator, Int)]

-- retrieve the average value of a list
mean :: [Int] -> Double
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
        samePlayers [x] = True
        samePlayers (x:y:zs) = (x == y) && samePlayers zs

        -- arrangements of players in several types of certain length
        -- could exist entities missing certain types
        playerArrangement :: Int -> Int -> [[Int]]
        playerArrangement 0 _ = [[]]
        playerArrangement players playerTypes = [x:xs | x <- [0 .. (playerTypes - 1)], xs <- playerArrangement (players - 1) playerTypes]

-- given a list of player's settings, arrange them ensuring all players are involved in the game
generatePlayerList :: Int -> PlayerList -> [PlayerList]
generatePlayerList pn ps = let pt = length ps
                               pl = validPlayerList pn pt
                           in  map (findItemByIdx ps) pl
    where
        findItemByIdx :: [a] -> [Int] -> [a]
        findItemByIdx _ [] = []
        findItemByIdx ps (i:is) = ps !! i:findItemByIdx ps is

-- there are totally fix evaluators, but here the random evaluator is ignored
-- in a three players game there exist 48 combinations for four evaluator's players to be invovled
{-
p34 :: [PlayerList]
p34 = generatePlayerList 3 [(Move, 0), (Board, 0), (MParanoid, 2), (MBRS, 2)]
p32 :: [PlayerList]
p32 = generatePlayerList 3 [(MParanoid, 2), (MParanoid, 3), (MBRS, 2), (MBRS, 3)]
-}

-- 18 combinations, could be divided into 3 groups with each 6 subsets
testSet :: Int -> [PlayerList]
testSet idx = chunksOf 6 (generatePlayerList 3 [(Move, 0), (Board, 0), (Random, 0)]) !! idx

--Run Experimental Trials---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- given a certain status, and run a game with different players till one of the players wins, and return the data for evaluating the performance
-- besides, one thing to point out is that the history trace and the killer moves are maintained by each player, in other words, they are not shared
singleRun :: [[Int]] -> (Maybe Int, Maybe Pico) -> PlayerIndex -> Board -> [[Pos]] -> Int -> [HistoryTrace] -> (Double, Double) -> [[KillerMoves]] -> PlayerList -> IO (PlayerIndex, [[Int]])
singleRun record control pi eboard iboards pn hts cons kms pl =
                                    -- there might exist cycling where all players are trying to maintain a board state that is most benefical for them
                                    -- therefore, it is necessary to have a mechanism to break the loop 
                                    if getTurns (length record) pn >= 500 then do printEoard eboard
                                                                                  error "Exceed!"
                                    else do gen <- newStdGen
                                            (neboard, niboard, nht, nkm, trials) <- finalSelection (GRoot 0 []) (gen, pi, 1, eboard, iboards, pn, hts !! pi, cons, sim) control
                                            -- system "cls"
                                            -- printEoard neboard
                                            let newRecord = replace pi (trials:(record !! pi)) record
                                                newHistory = replace pi nht hts
                                                newKillerMoves = replace pi nkm kms
                                            if winStateDetect niboard then return (pi, map reverse newRecord)
                                            else let niboards = replace pi niboard iboards
                                                     nextTurn = turnBase pn pi
                                                 in  niboards `par` nextTurn `pseq`
                                                     singleRun newRecord control nextTurn neboard niboards pn newHistory cons newKillerMoves pl
    where
        sim = let (x, y) = pl !! pi
              in  (x, y, kms !! pi)


-- start the game from the initial board state
-- from here, one phenomenon could be found, that the performed playouts were increasing as the game progressing, this could be because that when the game is close to 
-- the end state, there are no many effective move can be played, therefore, an iteration is stopped very fast and lead to an increasing number of playouts
runFromInitialState :: (Maybe Int, Maybe Pico) -> PlayerList -> IO ((PlayoutEvaluator, Int), [[Int]])
runFromInitialState control pl = do (winIdx, newRecord) <- singleRun record control 0 eboard iboards pn hts (3, 0.9) kms pl
                                    return (pl !! winIdx, newRecord)
    where
        pn = length pl
        record = replicate pn []
        eboard = eraseBoard (playerColourList pn) externalBoard
        iboards = replicate pn startBase
        hts = replicate pn RBLeaf
        kms = replicate pn (replicate pn [])

-- run a game with certain setting several times and return lists of winner players and the playouts taken from the game
multipleRuns :: Int -> (Maybe Int, Maybe Pico) -> PlayerList -> IO ([(PlayoutEvaluator, Int)], [[Int]])
multipleRuns runs control pl = do ls <- multipleRuns' runs control pl
                                  let winners = map fst ls
                                      trials = transpose' $ map snd ls
                                  return (winners, trials)
    where
        multipleRuns' 0 _ _ = return []
        multipleRuns' runs control pl = do item <- runFromInitialState control pl
                                           items <- multipleRuns' (runs - 1) control pl
                                           return $ item:items

-- gather the lists of the lists based on index/order
transpose' :: [[[b]]] -> [[b]]
transpose' ([]:_) = []
transpose' x = concatMap head x : transpose' (map tail x)

-- run several sets with different player settings and each sets contain multiple runs
multipleGames :: Int -> (Maybe Int, Maybe Pico) -> [PlayerList] -> IO [([(PlayoutEvaluator, Int)], [[Int]])]
multipleGames _ _ [] = return []
multipleGames runs control (pl:pls) = do result <- multipleRuns runs control pl
                                         rest <- multipleGames runs control pls
                                         return $ result:rest

-- testList = [([(Move, 0)], [[1,2,3], [4,5,6], [7,8,9]]), ([(Board, 0)], [[1,2,3], [4,5,6], [7,8,9]])]

merge :: [([a], [[b]])] -> ([a], [[b]])
merge [] = ([], [])
merge xs = let winners = map fst xs
               iters = map snd xs
           in  (concat winners, transpose' iters)

-- write the input to a certain file of given filename
experimentRecord :: Show a => [a] -> FilePath -> IO ()
experimentRecord xs filePath = do filePath <- openFile filePath WriteMode
                                  hPutStr filePath (convertToString xs)
                                  hClose filePath
                                  return ()
    where
        convertToString [] = ""
        convertToString ts = show (take 100 ts) ++ "\n" ++ convertToString (drop 100 ts)

-- ghc -main-is Experiment Experiment.hs -O2 -outputdir dist
-- this is just for testing purpose, run game several times with the fixed setting
main :: IO ()
main = do arg <- getArgs
          start <- lookupTable `seq` getCurrentTime
        {-
          (winner, counts) <- multipleRuns 10 (Nothing, Just 0.05) [(Move, 0), (Board, 0), (MParanoid, 2), (MBRS, 2)]
          let filePath = "./experiments/testTrial.txt"
          experimentRecord counts filePath
       -}
          results <- multipleGames 2 (Nothing, Just 0.05) (testSet 0)
          print $ merge results
          end <- getCurrentTime
          putStrLn $ "Time cost: " ++ show (diffUTCTime end start)
