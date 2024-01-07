module NBFS where
-- in order to complete a lookup table for evaluating the board state, the minimum moves for each board to reach the goal state is necessary
-- hence, this module aims to search the sufficient moves of any state for a player to win the game in single-agent board
-- if you're not interested in how to retrieve the lookup table used in this application, you can ignore this module

import NBoard ( occupiedBoardSize, Pos, borderCheck, extractJustValues, setNub, replace2, replace )
import Data.List ( elemIndex, sort, sortBy, transpose, nub )
import Control.Monad.ST ( runST )
import Data.STRef ( modifySTRef, newSTRef, readSTRef )
import Control.Monad.State ( State, evalState, MonadState(get) )
import Control.Monad.Extra ( concatMapM )
import Control.Parallel ( par, pseq )
import Data.Containers.ListUtils ( nubOrd )
import NProjection (goalBase, startBase)
import Control.Parallel.Strategies (parMap, rseq)
import qualified Data.Heap as Heap
import Data.Time (getCurrentTime, diffUTCTime)
import System.Random ( newStdGen, Random(randomRs, random), StdGen, mkStdGen, getStdGen )
import System.Environment (getArgs)
import qualified Data.Set as Set
import NZobrist (Hash, hashBoard)

-- it will need a breadth-first search for preventing duplicate moves, and also, due to the difficulty of measuring the prediction in A star, BFS is somehow more accurate 
-- each level represents a move that is done only if certain moves before were done
-- level 0 means the initial state
-- level 1 means a move is performed by each piece from the initial state, might exists duplications
-- as shown below:

testBoard :: [[Int]]
testBoard = [
        [0,0,0,0,1,1,1],
        [0,0,0,0,0,1,1],
        [0,0,0,0,0,0,1],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0]
    ]

printBoard' :: [Pos] -> IO ()
printBoard' ps = printEoard' $ transform2Table ps emptyTable
    where
        emptyTable :: [[Int]]
        emptyTable = replicate 7 $ replicate 7 0

        printEoard' :: [[Int]] -> IO ()
        printEoard' [] = return ()
        printEoard' (b:bs) = do print b
                                printEoard' bs

        transform2Table :: [Pos] -> [[Int]] -> [[Int]]
        transform2Table [] b = b
        transform2Table (p:ps) board = transform2Table ps (replace2 p 1 board)

randomBoardIO :: IO [(Int, Int)]
randomBoardIO = do gen1 <- newStdGen
                   gen2 <- newStdGen
                   let xs = randomRs (0, 6) gen1
                       ys = randomRs (0, 6) gen2
                       ps = take 6 . nub $ zip xs ys
                   return ps

randomBoard :: (Int, Int) -> [(Int, Int)]
randomBoard (seed1, seed2) = let xs = randomRs (0, 6) (mkStdGen seed1)
                                 ys = randomRs (0, 6) (mkStdGen seed2)
                                 ps = take 6 . nub $ zip xs ys
                             in  ps

randomBoards :: Int -> [[(Int,Int)]]
randomBoards 0 = []
randomBoards count = randomBoard (count, -count):randomBoards (count-1)

-- to speed up the processing, rather than having the whole occupied board, only the occupied positions are maintained in the state monad
-- additional consideration for more accurate estimate might be considered in multi-agent form, but not necessary here

-- a generation/layer of the search is consist of a list of candidate and corresponding centroid  
type MinLayer = Heap.MinPrioHeap Int [Pos]
type MaxLayer = Heap.MaxPrioHeap Int [Pos]
type TabuList = Set.Set Hash


-- ghc -main-is NBFS NBFS.hs -O2 -threaded -outputdir dist -fllvm
main :: IO()
main = do (size:boards:args) <- getArgs
          start <- getCurrentTime
          gen <- getStdGen
          let testBoards = randomBoards $ read boards
              shortestPaths = parMap rseq (shortestMoves (read size) gen) testBoards
          --   printBoard' test
          --   putStrLn $ "The Centroid is:" ++ show (centroid test) 
          putStrLn $ "The total moves: " ++ show (sum shortestPaths)
          end <- getCurrentTime
          putStrLn $ "Time cost: " ++ show (diffUTCTime end start)

-- discover a board's shortest path toward the goal state
shortestMoves :: Int -> StdGen -> [Pos] -> Int
shortestMoves wd gen ps = let evalValue = centroid ps
                          in  if evalValue == 28 then 0
                              else bSearch 0 wd gen 1 (Heap.singleton (evalValue, ps)) (Set.singleton (hashBoard ps)) Heap.empty

-- breadth-first search, heuristic-based
bSearch :: Int -> Int -> StdGen -> Float -> MinLayer -> TabuList -> MinLayer -> Int
bSearch i lim gen temp current record new =
                case Heap.view current of
                    -- turn to the next generation if the current states are all traversed
                    -- reset the buffer while remaining/cleaning the tabu list
                    Nothing -> {-if (i+1) `mod` 4 == 0 then-} bSearch (i+1) lim gen newTemp new record Heap.empty
                               -- else bSearch (i+1) lim gen newTemp new record Heap.empty
                    -- expand a board state in the current generation and update to the next one
                    Just ((c, b), rest) -> let candidates = expands (c, b) (allDestinations' b)
                                               (newLayer, newRecord, newGen) = renewLayer lim gen temp new record candidates
                                           in  -- determine if reaching the goal state, if so, then return the number of the total layers
                                               -- since the goal state is reached at the next step, the layer level needs to be incremented by 1
                                               if goalReached newLayer then i+1
                                               -- otherwise, keep investigating the next board state
                                               else bSearch i lim newGen temp rest record {-newRecord-} newLayer
    where
        goalReached layer = case Heap.viewHead layer of
                                Just (28, _) -> True
                                _ -> False

        newTemp = 0.95 * temp

-- Simulated annealing
-- Tabu Search
-- Breadth-First Search
-- popular the candidate movements within a certain width, also replace the board states with better ones once the width is reached
renewLayer :: Int -> StdGen -> Float -> MinLayer -> TabuList -> MaxLayer -> (MinLayer, TabuList, StdGen)
renewLayer lim gen temp minHp record maxHp =
    case Heap.view maxHp of
        -- base case 1, stop the renewng when finish updating the layer
        Nothing -> (minHp, record, gen)
        -- base case 2, stop when discover a goal state (will be seen firstly based on the nature of max-heap)
        Just (item@(28, _), _) -> (Heap.singleton item, record, gen)
        -- otherwise, compare the two candidates and make update
        Just (item@(maxcen, board), restMaxHp)
            -> let h = hashBoard board
               in  case Heap.view minHp of
                        -- fill the new item into the empty layer
                        Nothing -> renewLayer lim gen temp (Heap.insert item minHp) record {-(Set.insert h record)-} restMaxHp
                        Just ((mincen, _), restMinHp)
                            -- first discover whether this candidate has appeared before, if so, then skip this one 
                            -- | h `Set.member` record -> renewLayer lim gen temp minHp record restMaxHp
                            -- second, check the size of the layer, if not yet reached the given limit, then just update it
                            | Heap.size minHp < lim -> renewLayer lim gen temp (Heap.insert item minHp) record {-(Set.insert h record)-} restMaxHp
                            -- thrid, compare the two candidates, only update if giving positive increment of the centroid
                            -- since the two heap are different, one is minimum and another is maximum, therefore, the maximum candidate 
                            -- will always be compared with the minimum candidates, increasing the chance being accepted while eliminating the 
                            -- less promising ones
                            | maxcen >= mincen -> renewLayer lim gen temp (Heap.insert item restMinHp) record {-(Set.insert h record)-} restMaxHp
                            -- otherwise, try the SA condition 
                            | otherwise -> let (r, newGen) = random gen :: (Float, StdGen)
                                           in  -- generate a value to loosen constraint of the accepted candidates
                                               if r <= exp (fromIntegral (maxcen - mincen) / temp) then renewLayer lim newGen temp (Heap.insert item restMinHp) record {-(Set.insert h record)-} restMaxHp
                                               else renewLayer lim newGen temp minHp record restMaxHp

--Board Handling--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- to tend the movement from right-top to left-bottom, the centroid of the position can be set as: y-x
-- the centroid is used as the board evaluation for the shortest path search that the closer it is to the goal state, the larger reflect is given
-- such that the the closer to the home base, the larger the centroid is
-- calculate the centroid for the whole occupied board
centroid :: [Pos] -> Int
centroid ps = sum $ map centroidPos ps -- the highest value will be 28 while the lowest is -28
centroidPos :: Pos -> Int
centroidPos (x, y) = x - y
-- update a given centroid based on a movement
updateCentroid :: Int -> (Pos, Pos) -> Int
updateCentroid cen (from, to) = cen - centroidPos from + centroidPos to

-- the two types of symmetric a board could be presented
-- symmetric1 :: OccupiedBoard -> OccupiedBoard
-- symmetric1 = reverse . transpose . reverse
-- symmetric2 :: OccupiedBoard -> OccupiedBoard
-- symmetric2 = transpose

-- the symmetric conversion based on occupied positions 
-- horizontal 
symmetric1_pos :: [Pos] -> [Pos]
symmetric1_pos = map (\(x, y) -> (6 - y, 6 - x))
-- vertical
symmetric2_pos :: [Pos] -> [Pos]
symmetric2_pos = map (\(x, y) -> (y, x))

-- implement a list of movements and get the resulting boards (positions), combine all into a MinHeap
-- as well as update the new evaluation value of the resulting board
expands :: (Int, [Pos]) -> [(Pos, [Pos])] -> MaxLayer
expands currentState = Heap.fromList . concat . parMap rseq (flipBoard currentState)

-- generate new boards based on the given pair of movements: (pos, a list of destinations)
flipBoard :: (Int, [Pos]) -> (Pos, [Pos]) -> [(Int, [Pos])]
flipBoard _ (_, []) = []
flipBoard (cen, board) (p, m:ms) = case elemIndex p board of
                                        Nothing -> [] -- error "Invalid position for board flipping"
                                        Just idx -> let newBoard = replace idx m board
                                                        newCentroid = updateCentroid cen (p, m)
                                                    in  (newCentroid, newBoard):flipBoard (cen, board) (p, ms)

--Movement Operators-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- similar to the Board module but with different formats (Pos) and the board is simpler since there is no need to consider the colour issues
-- given a list of positions, find their corresponding available movements

-- offer a (only one) player's all avaiable moves on the given board
-- might cause error if contains multiple player's pieces
allDestinations' :: [Pos] -> [(Pos, [Pos])]
allDestinations' board = setNub $ parMap rseq (`destinations'` board) board

-- enter a board position and get a list of available movements/reachable positions: adjacent jump and chained jump, zipped with the given position
destinations' :: Pos -> [Pos] -> (Pos, [Pos])
destinations' pos board = if pos `elem` board
                          then let -- find the reachable destinations for steps
                                   js = jumps' pos board
                                   -- find the reachable destinations for hops
                                   hs = allHops' [] board pos
                               in  -- combine the two lists and discard the repeated ones
                                   (pos, js `par` hs `pseq` setNub (js ++ hs))
                          else (pos, [])  -- invalid position, therefore, empty list

adjacentPositions' :: [Pos -> Pos]
adjacentPositions' = [f (-1, 0), f (-1, -1), f (0, -1), f (1, 0), f (1, 1), f (0, 1)]
    where
        f (a, b) (x, y) = (a+x, b+y)

jumpDirections' :: Pos -> [Pos]
jumpDirections' ps = adjacentPositions' <*> pure ps

-- discover the available adjacent positions around the entered one
jumps' :: Pos -> [Pos] -> [Pos]
jumps' pos board = filter (\x -> x `notElem` board && validMove' pos x) (jumpDirections' pos)

-- generate a list of one-over jump chained together to reach a larger jump range
-- recursively search for the reachable destinations for chained jumps of different directions
-- during the search, a list of discovered positions is maintained to avoid cycling/repetition, 
-- while another list stores the new frontiers based on the previous list of positions
allHops' :: [Pos] -> [Pos] -> Pos -> [Pos]
allHops' record board ps = -- generate a new list of positions found
                           let moves = hops' ps board
                               -- add the newly discovered positions to the record, also avoid backward moves
                               newGeneration = filter (`notElem` record) moves
                               -- continue the next "layer" of search based on the positions discovered at this "layer"
                               newRecord = newGeneration ++ record
                               -- until not new positions are found, return the combined list of all found positions
                               expandedMoves = concat $ parMap rseq (allHops' newRecord board) newGeneration
                              -- might exist duplicated positions, but will be omitted at the final combination
                           in  expandedMoves ++ newGeneration

-- list a positions that can be reached by one hop
hops' :: Pos -> [Pos] -> [Pos]
hops' ps board = let -- generate a list of positions that could be reached by just one hop in different directions
                     moves = map (hopCheck' ps board) adjacentPositions'
                     -- discard the invalid positions
                 in  extractJustValues moves

-- given a board position, chek if a hop is valid in a certain direction
hopCheck' :: Pos -> [Pos] -> (Pos -> Pos) -> Maybe Pos
hopCheck' pos board f = if isOccupied ps1 && not (isOccupied ps2) && validMove' pos ps2 then Just ps2 else Nothing
        where
            ps1 = f pos
            ps2 = f ps1

            isOccupied x = x `elem` board

-- check the validity of the movement
validMove' :: Pos -> Pos -> Bool
validMove' from to = borderCheck' to && baseMoveAllow' from to

borderCheck' :: Pos -> Bool
borderCheck' = NBoard.borderCheck (occupiedBoardSize, occupiedBoardSize)

-- prevent piece from moving out the goal base
baseMoveAllow' :: Pos -> Pos -> Bool
baseMoveAllow' from to = (from `notElem` goalBase) || (to `elem` goalBase)
