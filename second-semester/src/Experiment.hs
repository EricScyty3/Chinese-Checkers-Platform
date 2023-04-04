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
-- the thrid is the (median, average) turns that takes to end the game during the experiments
-- besides, the game is also limited to avoid endless cycle, and when this is occurred, the evalutor will be applied to determine the win for each player

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
singleRun :: [Board] -> (Maybe Int, Maybe Pico) -> PlayerIndex -> Board -> [[Pos]] -> Int -> [HistoryTrace] -> (Double, Double) -> [[KillerMoves]] -> PlayerList -> IO (PlayerIndex, Int)
singleRun record control pi eboard iboards pn hts cons kms pl =
                                    -- there might exist cycling where all players are trying to maintain a board state that is most benefical for them
                                    -- therefore, it is necessary to have a mechanism to break the loop 
                                    if getTurns (length record) pn >= 500 then do printEoard eboard
                                                                                  error "Exceed!"
                                    else do gen <- newStdGen
                                            (neboard, niboard, nht, nkm) <- finalSelection (GRoot 0 []) (gen, pi, 1, eboard, iboards, pn, hts !! pi, cons, sim) control
                                            -- system "cls"
                                            -- printEoard neboard
                                            let newRecord = neboard:record
                                            if winStateDetect niboard then return (pi, getTurns (length newRecord) pn)
                                            else let niboards = replace pi niboard iboards
                                                     nextTurn = turnBase pn pi
                                                 in  niboards `par` nextTurn `pseq`
                                                     singleRun newRecord control nextTurn neboard niboards pn (replace pi nht hts) cons (replace pi nkm kms) pl
    where
        sim = let (x, y) = pl !! pi
              in  (x, y, kms !! pi)


-- start the game from the initial board state
runFromInitialState :: (Maybe Int, Maybe Pico) -> PlayerList -> IO ((PlayoutEvaluator, Int), Int)
runFromInitialState control pl = do (winIdx, turns) <- singleRun [] control 0 eboard iboards pn hts (3, 0.5) kms pl
                                    return (pl !! winIdx, turns)
    where
        pn = length pl
        eboard = eraseBoard (playerColourList pn) externalBoard
        iboards = replicate pn startBase
        hts = replicate pn RBLeaf
        kms = replicate pn (replicate pn [])

        turn2Sname :: (PlayoutEvaluator, Int) -> String
        turn2Sname (Random, _) = "R"
        turn2Sname (Move, _) = "M"
        turn2Sname (Board, _) = "B"
        turn2Sname (MParanoid, depth) = "MP" ++ show depth
        turn2Sname (MBRS, depth) = "MS" ++ show depth


-- run a game with same settings several times and return lists of winner and taken turns to complete the game
multipleRuns :: Int -> (Maybe Int, Maybe Pico) -> PlayerList -> IO [((PlayoutEvaluator, Int), Int)]
multipleRuns 0 _ _ = return []
multipleRuns runs control pl = do item <- runFromInitialState control pl
                                  items <- multipleRuns (runs - 1) control pl
                                  return $ item `par` items `pseq` item:items

-- run several sets with different player settings and each sets contain multiple runs
multipleGames :: Int -> (Maybe Int, Maybe Pico) -> [PlayerList] -> IO [((PlayoutEvaluator, Int), Int)]
multipleGames _ _ [] = return []
multipleGames runs control (pl:pls) = do result <- multipleRuns runs control pl
                                         rest <- multipleGames runs control pls
                                         return $ result `par` rest `pseq` result ++ rest

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
main :: IO ()
main = do arg <- getArgs
          start <- lookupTable `seq` getCurrentTime
          
          let {-idx = read (arg !! 0)
              runs = read (arg !! 1)
              control = read (arg !! 2)
            --   pl = read (arg !! 3)
              runSet = head $ testSet idx-}
          
        --   print runSet
          results <- multipleGames 20 (Just 10, Nothing) [[(Move, 0), (Board, 0), (Move, 0)]]
          let filePath = "./experiments/testTrial" ++ ".txt"
          experimentRecord results filePath
          end <- getCurrentTime
          putStrLn $ "Time cost: " ++ show (diffUTCTime end start)
