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
-- import Control.Parallel.Strategies (using, parList, rseq, parListChunk, parMap)

-- during the experimental trials, several outcomes are measured only under the contorl of time limits
-- the first is the total win rate in the multiple players game, either only two player types are invovled or the same number of the total players
-- the second is the (median) playouts, or more specific, the iterations that could be performed under the set time

-- a list of players' settings to be invovled in the experimental trials, containing the evaluator to be used during the playout phase 
-- as well as the search depth
type Player = (PlayoutEvaluator, Int)

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

-- a list of tested player settings
standardOrder :: [Player]
standardOrder = [(Random, 0), (Move, 0), (Board, 0), (MParanoid, 2), (MParanoid, 3), (MBRS, 2), (MBRS, 3)]

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
playerList2 :: Int -> Int -> [[Int]]
playerList2 pn pt = filter ifOnlyTwoPlayers (validPlayerList pn pt)
    where
        ifOnlyTwoPlayers :: [Int] -> Bool
        ifOnlyTwoPlayers xs = length (nub xs) == 2

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

-- given a certain status, and run a game with different players till one of the players wins, and return the data for evaluating the performance
-- besides, one thing to point out is that the history trace and the killer moves are maintained by each player, in other words, they are not shared
singleRun :: [[Int]] -> MCTSControl -> PlayerIndex -> Board -> [[Pos]] -> Int -> [HistoryTrace] ->
             (Double, Double) -> [[KillerMoves]] -> [Player] -> Int -> IO (Maybe (PlayerIndex, [[Int]]))
singleRun record control pi eboard iboards pn hts cons kms pl counts =
                                    -- there might exist cycling where all players are trying to maintain a board state that is most benefical for them
                                    -- therefore, it is necessary to have a mechanism to break the loop 
                                    if getTurns counts pn >= 500 then return Nothing
                                    else do gen <- newStdGen
                                            (neboard, niboard, nht, nkm, playouts) <- finalSelection (GRoot 0 []) (gen, pi, 1, eboard, iboards, pn, hts !! pi, cons, sim) control
                                            let newRecord = replace pi (playouts:(record !! pi)) record
                                                newHistory = replace pi nht hts
                                                newKillerMoves = replace pi nkm kms
                                            -- system "cls"
                                            -- printEoard neboard
                                            if winStateDetect niboard then return $ Just (pi, map reverse newRecord)
                                            else let niboards = replace pi niboard iboards
                                                     nextTurn = turnBase pn pi
                                                 in  niboards `par` nextTurn `pseq`
                                                     singleRun newRecord control nextTurn neboard niboards pn newHistory cons newKillerMoves pl (counts+1)
    where
        sim = let (x, y) = pl !! pi
              in  (x, y, kms !! pi)


-- start the game from the initial board state
-- from here, one phenomenon could be found, that the performed playouts were increasing as the game progressing, this could be because that when the game is close to 
-- the end state, there are no many effective move can be played, therefore, an iteration is stopped very fast and lead to an increasing number of playouts
runFromInitialState :: MCTSControl -> [Player] -> IO (Player, [[Int]])
runFromInitialState control pl = do result <- singleRun record control 0 eboard iboards pn hts (0.5, 5) kms pl 0
                                    case result of
                                        Nothing -> runFromInitialState control pl -- rerun the experiment if cycle exists
                                        Just (winIdx, newRecord) -> return (pl !! winIdx, reOrder (replicate (length standardOrder) []) newRecord pl)
                                    -- an additional reorder action is taken place here, where the players are ordered as 
    where
        pn = length pl
        record = replicate pn []
        eboard = eraseBoard (playerColourList pn) externalBoard
        iboards = replicate pn startBase
        hts = replicate pn RBLeaf
        kms = replicate pn (replicate pn [])

        reOrder :: [[Int]] -> [[Int]] -> [Player] -> [[Int]]
        reOrder ls [] _ = ls
        reOrder ls _ [] = ls
        reOrder ls (x:xs) (p:ps) = case p `elemIndex` standardOrder of
                                    Nothing -> error "Invalid Player"
                                    Just idx -> let item = ls !! idx
                                                    nitem = item ++ x
                                                in  reOrder (replace idx nitem ls) xs ps

-- run a game with certain setting several times and return lists of winner players and the playouts taken from the game
multipleRuns :: Int -> MCTSControl -> [Player] -> IO ([Player], [[Int]])
multipleRuns runs control pl = let pls = replicate runs pl
                               in  do ls <- mapM (runFromInitialState control) pls
                                      let winners = map fst ls
                                          playouts = transpose $ map snd ls
                                      return (winners, playouts)

-- gather the lists of the lists based on index/order
transpose :: [[[b]]] -> [[b]]
transpose ([]:_) = []
transpose x = concatMap head x : transpose (map tail x)

-- run several sets with different player settings and each sets contain multiple runs
multipleGames :: Int -> MCTSControl -> [[Player]] -> IO ([Player], [[Int]])
multipleGames runs control pls = do results <- mapConcurrently (multipleRuns runs control) pls
                                    return $ merge results


-- testList = [([(Move, 0)], [[1,2,3], [4,5,6], [7,8,9]]), ([(Board, 0)], [[1,2,3], [4,5,6], [7,8,9]])]
merge :: [([a], [[b]])] -> ([a], [[b]])
merge [] = ([], [])
merge xs = let winners = map fst xs
               iters = map snd xs
           in  (concat winners, transpose iters)

-- write the input to a certain file of given filename
experimentRecord :: (Show a, Show b) => ([a], [[b]]) -> FilePath -> IO ()
experimentRecord (xs, ys) fileName = do path1 <- openFile playoutsFile WriteMode
                                        hPutStr path1 (convertToStrings ys)
                                        hClose path1

                                        path2 <- openFile winnersFile WriteMode
                                        hPutStr path2 (convertToString xs)
                                        hClose path2

                                        return ()
    where
    playoutsFile = fileName ++ "_playouts.txt"
    winnersFile = fileName ++ "_winners.txt"

    convertToStrings [] = ""
    convertToStrings (x:xs) = (if null x then "[]\n" else convertToString x) ++ "\n" ++ convertToStrings xs

    convertToString [] = ""
    convertToString ts = show (take 100 ts) ++ "\n" ++ convertToString (drop 100 ts)


-- ghc -main-is Experiment Experiment.hs -O2 -threaded -outputdir dist
-- this is just for testing purpose, run game several times with the fixed setting
main :: IO ()
main = do arg <- getArgs
          start <- lookupTable `seq` getCurrentTime

          let control@(iterations, time) = read $ head arg :: MCTSControl -- time: from 0.005s to 0.05s, and finally 0.5s, 
                                                                          -- iteration: from 10 to 50 and finally to 100 (may not be considered)
              idx  = read $ arg !! 1 :: Int
              str = printf "%.3f" (fromMaybe 0 time)
              fileName = "./experiments/test1/" ++ str ++ "_" ++ show idx
              testSet  = generatePlayerList 3 [(Move, 0), (Board, 0), (MParanoid, 2), (MBRS, 2)] -- 60 combinations

          result <- multipleGames 100 control (divide2Chunks 10 testSet idx) 
          experimentRecord result fileName
          end <- result `seq` getCurrentTime
          putStrLn $ "Time cost: " ++ show (diffUTCTime end start)
          putStrLn "Completed"

-- divide the player arrangements into several smaller sets and pick one of them
divide2Chunks :: Int -> [[Player]] -> Int -> [[Player]]
divide2Chunks chunks ps idx = chunksOf chunkSize ps !! idx
    where
        chunkSize = length ps `div` chunks

{-
calculateWinRate :: Double -> [Int] -> Int -> IO [Double]
calculateWinRate time indices fileIndex = do winners <- loadWinners time indices fileIndex 
                                             return $ calculateWinRate' standardOrder (concat winners)
    where
        calculateWinRate' :: [Player] -> [Player] -> [Double]
        calculateWinRate' [] _ = []
        calculateWinRate' (p:ps) xs = fromIntegral (length (p `elemIndices` xs)) / fromIntegral (length xs) : calculateWinRate' ps xs

-- loadPlayouts :: Double -> [Int] -> Int -> IO [[Int]]
-- loadPlayouts _ [] _ = return []
-- loadPlayouts time (i:is) fileIndex = do playouts <- loadExperimentData fileName :: IO [[Int]]
--                                         rest <- loadPlayouts time is fileIndex
--                                         return (playouts ++ rest)
--     where
--         str = printf "%.3f" time
--         fileName = "./experiments/test" ++ show fileIndex ++ "/" ++ str ++ "_" ++ show i ++ "_playouts.txt"


loadWinners :: Double -> [Int] -> Int -> IO [[Player]]
loadWinners _ [] _ = return []
loadWinners time (i:is) fileIndex = do winners <- loadExperimentData fileName :: IO [[Player]]
                                       rest <- loadWinners time is fileIndex
                                       return (winners ++ rest)
    where
        str = printf "%.3f" time
        fileName = "./experiments/test" ++ show fileIndex ++ "/" ++ str ++ "_" ++ show i ++ "_winners.txt"


loadExperimentData :: Read b => FilePath -> IO [b]
loadExperimentData fileName = do filePath <- openFile fileName ReadMode
                                 contents <- hGetContents filePath
                                 return $ convertToElement (lines contents)
    where
        convertToElement s = map read s
-}