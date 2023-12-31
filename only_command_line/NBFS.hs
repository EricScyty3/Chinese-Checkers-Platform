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
import System.Random ( newStdGen, Random(randomRs) )
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

randomBoard :: IO [(Int, Int)]
randomBoard = do gen1 <- newStdGen
                 gen2 <- newStdGen
                 let xs = randomRs (0, 6) gen1
                     ys = randomRs (0, 6) gen2
                     ps = take 6 . nub $ zip xs ys
                 return ps

-- to speed up the processing, rather than having the whole occupied board, only the occupied positions are maintained in the state monad
-- additional consideration for more accurate estimate might be considered in multi-agent form, but not necessary here

-- a generation/layer of the search is consist of a list of candidate and corresponding centroid  
type MinLayer = Heap.MinPrioHeap Int [Pos]
type MaxLayer = Heap.MaxPrioHeap Int [Pos]
type Record = Set.Set Hash

-- ghc -main-is NBFS NBFS.hs -O2 -threaded -outputdir dist
main :: IO()
main = do (size1:size2:args) <- getArgs
          start <- getCurrentTime
          test <- randomBoard
          let -- test = [(2,6),(3,3),(3,5),(6,4),(6,5),(6,6)]
              x = shortestMoves test (read size1)
              y = shortestMoves test (read size2)
          printBoard' test
          putStrLn $ "The Centroid is:" ++ show (centroid test) 
          x `par` y `pseq` putStrLn $ "The Shortest Path is: " ++ show x
          putStrLn $ "The Shortest Path is: " ++ show y
          end <- getCurrentTime
          putStrLn $ "Time cost: " ++ show (diffUTCTime end start)

-- discover a board's shortest path toward the goal state
shortestMoves :: [Pos] -> Int -> Int
shortestMoves ps wd = let evalValue = centroid ps
                      in  if evalValue == 28 then 0 
                          else bSearch 0 wd (Heap.singleton (evalValue, ps)) (Set.singleton (hashBoard ps)) Heap.empty

-- breadth-first search
bSearch :: Int -> Int -> MinLayer -> Record -> MinLayer -> Int
bSearch i lim current record new = 
                case Heap.view current of
                    -- turn to the next generation if the current states are all traversed
                    -- reset the record and the buffer
                    Nothing -> bSearch (i+1) lim new Set.empty Heap.empty
                    -- expand a board state in the current generation and update to the next one
                    Just ((c, b), rest) -> let candidates = expands (c, b) (allDestinations' b)
                                               (newLayer, newRecord) = renewLayer lim new record candidates
                                           in  -- determine if reaching the goal state, if so, then return the number of the total layers
                                               if goalReached newLayer then i+1
                                               -- otherwise, keep investigating the next board state
                                               else bSearch i lim rest newRecord newLayer
    where 
        goalReached layer = case Heap.viewHead layer of
                                Just (28, _) -> True
                                _ -> False
{-
bSearch2 :: Int -> Int -> MinLayer -> MinLayer -> MinLayer -> Int
bSearch2 level size old new1 new2 = 
    case Heap.view old of
        Nothing -> bSearch2 (level+1) size (Heap.union new1 new2) Heap.empty Heap.empty
        Just ((c, b), rest) -> case Heap.view rest of 
                                 Nothing -> let candidates = expands (c, b) (allDestinations' b)
                                                newLayer = renewLayer size new1 candidates
                                            in  if goalReached newLayer then level+1
                                                else bSearch2 (level+1) size (Heap.union newLayer new2) Heap.empty Heap.empty
                                 Just ((c',b'), rest') -> let candidates1 = expands (c, b) (allDestinations' b)
                                                              newLayer1 = renewLayer size new1 candidates1
                                                              candidates2 = expands (c', b') (allDestinations' b')
                                                              newLayer2 = renewLayer size new2 candidates2
                                                          in  if newLayer1 `par` newLayer2 `pseq` (goalReached newLayer1 || goalReached newLayer2) then level+1
                                                              else bSearch2 level size rest' newLayer1 newLayer2
-}

        

-- -- for a list of board states, process each new state for each state 
-- -- update the new positions based on the old ones
-- -- finally return the level index, meaning the moves required to reach the goal state
-- bSearchS :: Int -> Int -> [([Pos], Int)] -> [([Pos], Int)] -> [([Pos], Int)] -> Int
-- bSearchS i wd np1 np2 [] = let combinedSet = runST $ do n <- newSTRef np2
--                                                         modifySTRef n (updateList np1 wd)
--                                                         readSTRef n
--                            in bSearchS (i+1) wd [] [] combinedSet -- clear the new position list and start searching on them, and update the level counts

-- bSearchS i wd np1 np2 [b] = case evalState (bSearch np1 wd) b of
--                                 [(_, 28)] -> i  -- indicate that the goal state is reached, return the level counts
--                                 ls -> bSearchS i wd ls np2 [] -- otherwise, keep searching/expanding the known board states

-- bSearchS i wd np1 np2 (b:bs) = let set1 = evalState (bSearch np1 wd) b -- bidirectional solution, that solves the boards in two direction in parallel to save the time
--                                    set2 = evalState (bSearch np2 wd) (last bs)
--                                in  if set1 `par` set2 `pseq` (goalReached set1 || goalReached set2) then i
--                                    else bSearchS i wd set1 set2 (init bs)
--     where
--         goalReached :: [([Pos], Int)] -> Bool
--         goalReached [(_, 28)] = True
--         goalReached _ = False

-- for each pieces on a board, return the resulting board states it could lead to
-- return the list containing the new found states and previously accumulated states
-- expandLayer :: (Int, [Pos]) -> Int -> MinLayer
-- expandLayer (cen, board) lim = 
-- bSearch :: [([Pos], Int)] -> Int -> State ([Pos], Int) [([Pos], Int)] -- maintaining the current board and its value
-- bSearch ps wd = do (board, score) <- get
--                    let moves = dListForBoard board -- retrieve the available moves
--                        ns = evalState (flipLists score moves) board -- result the expanded boards
--                    return (runST $ do n <- newSTRef ps
--                                       modifySTRef n (updateList ns wd)
--                                       readSTRef n) -- update the previously discovered board states with the new found board states

-- popular the candidate movements within a certain width, also replace the board states with better ones once the width is reached
renewLayer :: Int -> MinLayer -> Record -> MaxLayer -> (MinLayer, Record)
renewLayer lim minHp record maxHp =
    case Heap.view maxHp of
        Nothing -> (minHp, record) -- return the new candidate set
        Just (item@(28, _), _) -> (Heap.singleton item, record)  -- if reach the goal then just return a singleton
        Just (item@(maxcen, board), restMaxHp) -> 
                    let h = hashBoard board
                    in  -- if the heap's space is still spare then just insert the the candidate
                        -- need to make sure that such a board state is not discovered before in the current layer
                        -- skip if already exist
                        if h `Set.member` record then renewLayer lim minHp record restMaxHp
                        else if Heap.size minHp < lim then renewLayer lim (Heap.insert item minHp) (Set.insert h record) restMaxHp
                             -- otherwise, have to filter out the ones wth less centroid 
                             else case Heap.view minHp of
                                    Nothing -> error "Invalid operation in Heap"
                                    Just ((mincen, board'), restMinHp) -> 
                                                    if maxcen >= mincen 
                                                    then renewLayer lim (Heap.insert item restMinHp) (Set.insert h record) restMaxHp
                                                    else renewLayer lim minHp record restMaxHp

-- updateList (n:ns) wb ps
--   | snd n == 28 = [n] -- if reach the goal then just return the goal state
--   | n `elem` ps = updateList ns wb ps -- skip if already exists in the candidate set
--   | length ps < wb = updateList ns wb (mySort (n:ps)) -- just add if still has space
--   | otherwise = let minScore = (snd . head) ps -- the minimum item is the first item of the list
--                     (_, score) = n
--                 in  if score `par` minScore `pseq` (score > minScore) then updateList ns wb (mySort (n:tail ps)) -- replace the element with the current board state
--                     else updateList ns wb ps -- if the board state is not better then skip 

-- sort the list based on score, such that every time the minimum value is at the front
-- mySort :: [([Pos], Int)] -> [([Pos], Int)]
-- mySort = sortBy (\(_, x) (_, y) -> compare x y)

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
                                        Nothing -> error "Invalid position for board flipping"
                                        Just idx -> let newBoard = replace idx m board
                                                        newCentroid = updateCentroid cen (p, m)
                                                    in  (newCentroid, newBoard):flipBoard (cen, board) (p, ms)
-- expands _ [] = return []
-- expands v (x:xs) = do let (p, ps) = x
--                         resultingBoards <- mapM (flipBoardState v p) ps -- get the resulting boards and their values for the one position
--                         otherResultingBoards <- flipLists v xs -- the rest of the resulting boards as well as the corresponding values
--                         return (resultingBoards `par` otherResultingBoards `pseq` (resultingBoards ++ otherResultingBoards)) -- eventually get all possible boards result from the current board
--     where
--         -- exchange two pieces' states on the occupied board by replacing the position with a new one as well as update the new evaluation value
--         flipBoardState :: Int -> Pos -> Pos -> State [Pos] ([Pos], Int)
--         flipBoardState v f t = do ps <- get
--                                   case elemIndex f ps of
--                                     Nothing -> error "Cannot find this position in the occupied board"
--                                     Just i  -> do let newps = sort $ t : filter (/= f) ps
--                                                       newv  = v - centroidPos f + centroidPos t
--                                                   return (newps `par` newv `pseq` (newps, newv))

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

-- -- combine the two movement lists and discard the duplicate ones
-- destinationList' :: Pos -> State [Pos] (Pos, [Pos])
-- destinationList' p = do ps <- get
--                         if p `notElem` ps then return (p, []) -- invalid input position
--                         else do adjacentMoves <- findAvaliableNeighbors' p
--                                 chainedMoves  <- recursiveSearch' [] p
--                                 let movesList = adjacentMoves `par` chainedMoves `pseq` nubOrd (adjacentMoves ++ chainedMoves)
--                                 return (p, movesList) -- return the pair of source positions and a list of target positions

adjacentPositions' :: [Pos -> Pos]
adjacentPositions' = [f (-1, 0), f (-1, -1), f (0, -1), f (1, 0), f (1, 1), f (0, 1)]
    where
        f (a, b) (x, y) = (a+x, b+y)

jumpDirections' :: Pos -> [Pos]
jumpDirections' ps = adjacentPositions' <*> pure ps

-- discover the available adjacent positions around the entered one
jumps' :: Pos -> [Pos] -> [Pos]
jumps' pos board = filter (\x -> x `notElem` board && validMove' pos x) (jumpDirections' pos)

-- -- search for all chained jump destinations that can be reached
-- recursiveSearch' :: [Pos] -> Pos -> State [Pos] [Pos]
-- recursiveSearch' record pos = do chainJumpList <- jumpDirection' pos
--                                  let renewList = filter (`notElem` record) chainJumpList -- prevent duplicate search 
--                                      newRecord = renewList ++ record
--                                  recursiveList <- concatMapM (recursiveSearch' newRecord) renewList
--                                  return (recursiveList ++ renewList)

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

-- -- return the available hopping destinations
-- jumpDirection' :: Pos -> State [Pos] [Pos]
-- jumpDirection' pos = do reachableList <- mapM (determineValidJump' pos) [f (0, -1), f (-1, -1), f (-1, 0), f (0, 1), f (1, 1), f (1, 0)]
--                         return $ filter (/= pos) reachableList -- remove the invalid moves
--     where
--         f (a, b) (x, y) = (a+x, b+y)

-- given a board position, chek if a hop is valid in a certain direction
hopCheck' :: Pos -> [Pos] -> (Pos -> Pos) -> Maybe Pos
hopCheck' pos board f = if isOccupied ps1 && not (isOccupied ps2) && validMove' pos ps2 then Just ps2 else Nothing
        where
            ps1 = f pos
            ps2 = f ps1

            isOccupied x = x `elem` board

-- check if a one over hop is valid
-- determineValidJump' :: Pos -> (Pos -> Pos) -> State [Pos] Pos
-- determineValidJump' pos f = if not validMoveCheck then return pos
--                             else do ps <- get
--                                     if fp2 `notElem` ps && fp `elem` ps then return fp2
--                                     else return pos
--     where
--         fp = f pos
--         fp2 = (f . f) pos
--         size = occupiedBoardSize

--         -- check the validity of the movement
--         validMoveCheck :: Bool
--         validMoveCheck = testValidPos size size fp2 && baseMoveAllow' pos fp2

-- check the validity of the movement
validMove' :: Pos -> Pos -> Bool
validMove' from to = borderCheck' to && baseMoveAllow' from to

borderCheck' :: Pos -> Bool
borderCheck' = NBoard.borderCheck (occupiedBoardSize, occupiedBoardSize)

-- prevent piece from moving out the goal base
baseMoveAllow' :: Pos -> Pos -> Bool
baseMoveAllow' from to = (from `notElem` goalBase) || (to `elem` goalBase)
