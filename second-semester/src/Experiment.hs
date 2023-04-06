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
      Pos, removeByIdx )
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
import Data.List (elemIndices, sort, nub, permutations, elemIndex)
import Minimax (MGameTreeStatus)
import Control.Monad.State (evalState)
import Data.List.Extra (chunksOf)
import System.IO ( hClose, openFile, hPutStr, IOMode(WriteMode) )
import System.Process (system)
import Text.Printf (printf)


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

-- given a list of players, arrange them in several different orders for fair games
-- ensuring that the input players are not repeated, an additional check is applied here
generatePlayerList :: Int -> [Player] -> [[Player]]
generatePlayerList pn ps = let nps = nub ps
                               pt = length nps
                               pl = validPlayerList pn pt
                           in  map (findItemByIdx nps) pl
    where
        findItemByIdx :: [a] -> [Int] -> [a]
        findItemByIdx _ [] = []
        findItemByIdx ps (i:is) = ps !! i:findItemByIdx ps is

--Run Experimental Trials---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- given a certain status, and run a game with different players till one of the players wins, and return the data for evaluating the performance
-- besides, one thing to point out is that the history trace and the killer moves are maintained by each player, in other words, they are not shared
singleRun :: [[Int]] -> Pico -> PlayerIndex -> Board -> [[Pos]] -> Int -> [HistoryTrace] -> 
             (Double, Double) -> [[KillerMoves]] -> [Player] -> IO (PlayerIndex, [[Int]])
singleRun record time pi eboard iboards pn hts cons kms pl =
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
                                                     singleRun newRecord time nextTurn neboard niboards pn newHistory cons newKillerMoves pl
    where
        sim = let (x, y) = pl !! pi
              in  (x, y, kms !! pi)
        
        control = (Nothing, Just time)

-- start the game from the initial board state
-- from here, one phenomenon could be found, that the performed playouts were increasing as the game progressing, this could be because that when the game is close to 
-- the end state, there are no many effective move can be played, therefore, an iteration is stopped very fast and lead to an increasing number of playouts
runFromInitialState :: Pico -> [Player] -> IO (Player, [[Int]])
runFromInitialState time pl = do (winIdx, newRecord) <- singleRun record time 0 eboard iboards pn hts (3, 0.9) kms pl
                                 return (pl !! winIdx, reOrder (replicate (length standardOrder) []) newRecord pl)
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

        standardOrder = [(Random, 0), (Move, 0), (Board, 0), (MParanoid, 2), (MParanoid, 3), (MBRS, 2), (MBRS, 3)]


-- run a game with certain setting several times and return lists of winner players and the playouts taken from the game
multipleRuns :: Int -> Pico -> [Player] -> IO ([Player], [[Int]])
multipleRuns runs time pl = do ls <- multipleRuns' runs time pl
                               let winners = map fst ls
                                   playouts = transpose $ map snd ls
                               return (winners, playouts)
    where
        multipleRuns' 0 _ _ = return []
        multipleRuns' runs control pl = do item <- runFromInitialState control pl
                                           items <- multipleRuns' (runs - 1) control pl
                                           return $ item:items

-- gather the lists of the lists based on index/order
transpose :: [[[b]]] -> [[b]]
transpose ([]:_) = []
transpose x = concatMap head x : transpose (map tail x)


-- run several sets with different player settings and each sets contain multiple runs
multipleGames :: Int -> Pico -> [[Player]] -> IO [([Player], [[Int]])]
multipleGames _ _ [] = return []
multipleGames runs time (pl:pls) = do result <- multipleRuns runs time pl
                                      rest <- multipleGames runs time pls
                                      return $ result:rest

-- testList = [([(Move, 0)], [[1,2,3], [4,5,6], [7,8,9]]), ([(Board, 0)], [[1,2,3], [4,5,6], [7,8,9]])]
merge :: [([a], [[b]])] -> ([a], [[b]])
merge [] = ([], [])
merge xs = let winners = map fst xs
               iters = map snd xs
           in  (concat winners, transpose iters)

-- write the input to a certain file of given filename
experimentRecord :: (Show a, Show b) => ([a], [[b]]) -> FilePath -> IO ()
experimentRecord (xs, ys) filePath = do filePath <- openFile filePath WriteMode
                                        hPutStr filePath (convertToString xs)
                                        hPutStr filePath (convertToStrings ys)
                                        hClose filePath
                                        return ()
    where
        convertToStrings [] = ""
        convertToStrings (x:xs) = convertToString x ++ "\n" ++ convertToStrings xs

        convertToString [] = ""
        convertToString ts = show (take 100 ts) ++ "\n" ++ convertToString (drop 100 ts)
       

-- ghc -main-is Experiment Experiment.hs -O2 -outputdir dist
-- this is just for testing purpose, run game several times with the fixed setting
main :: IO ()
main = do arg <- getArgs
          start <- lookupTable `seq` getCurrentTime
          let time = read $ head arg :: Double
              run = read $ arg !! 1 :: Int
              idx = read $ arg !! 2 :: Int
          -- test tiral 0
          -- results <- multipleRuns run (realToFrac time) [(Move, 0), (Board, 0), (MParanoid, 2), (MBRS, 2)]
          -- test trial 1.0, 1.1, 1.2
          tempResults <- multipleGames run (realToFrac time) (testSet idx)
          let results = merge tempResults
              str = printf "%.2f" time
              filePath = "./experiments/testTrial1_" ++ show idx ++ "_" ++ str ++ ".txt"
          experimentRecord results filePath
          end <- getCurrentTime
          putStrLn $ "Time cost: " ++ show (diffUTCTime end start)


-- there are totally fix evaluators, but here the random evaluator is ignored
-- in a three players game there exist 48 combinations for four evaluator's players to be invovled
{-
p34 :: [PlayerList]
p34 = generatePlayerList 3 [(Move, 0), (Board, 0), (MParanoid, 2), (MBRS, 2)]
p32 :: [PlayerList]
p32 = generatePlayerList 3 [(MParanoid, 2), (MParanoid, 3), (MBRS, 2), (MBRS, 3)]
-}

-- 18 combinations, could be divided into 3 groups with each 6 subsets
-- here, three time limits are applied: 0.05s, 0.5s and 5s
testSet :: Int -> [[Player]]
testSet idx = chunksOf 6 (generatePlayerList 3 [(Move, 0), (Board, 0), (Random, 0)]) !! idx