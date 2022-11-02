module Board where

data BoardType = R | B | G | E | U deriving (Eq, Show)
type RowBoardState = [BoardType]

boardWidth :: Int
boardWidth = 21
boardHeight :: Int
boardHeight = 13
-- a two-dimension array representing the game board
myList :: [RowBoardState]
myList = [
   [U, U, U, U, U, U, U, U, U, U, R, U, U, U, U, U, U, U, U, U, U],
   [U, U, U, U, U, U, U, U, U, R, U, R, U, U, U, U, U, U, U, U, U],
   [U, U, U, U, U, U, U, U, R, U, R, U, R, U, U, U, U, U, U, U, U],
   [U, G, U, G, U, G, U, E, U, E, U, E, U, E, U, B, U, B, U, B, U],
   [U, U, G, U, G, U, E, U, E, U, E, U, E, U, E, U, B, U, B, U, U],
   [U, U, U, G, U, E, U, E, U, E, U, E, U, E, U, E, U, B, U, U, U],
   [U, U, U, U, E, U, E, U, E, U, E, U, E, U, E, U, E, U, U, U, U],
   [U, U, U, B, U, E, U, E, U, E, U, E, U, E, U, E, U, G, U, U, U],
   [U, U, B, U, B, U, E, U, E, U, E, U, E, U, E, U, G, U, G, U, U],
   [U, B, U, B, U, B, U, E, U, E, U, E, U, E, U, G, U, G, U, G, U],
   [U, U, U, U, U, U, U, U, R, U, R, U, R, U, U, U, U, U, U, U, U],
   [U, U, U, U, U, U, U, U, U, R, U, R, U, U, U, U, U, U, U, U, U],
   [U, U, U, U, U, U, U, U, U, U, R, U, U, U, U, U, U, U, U, U, U]]

myList2 :: [[Int]]
myList2 = [
   [00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 01, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00],
   [00, 00, 00, 00, 00, 00, 00, 00, 00, 02, 00, 03, 00, 00, 00, 00, 00, 00, 00, 00, 00],
   [00, 00, 00, 00, 00, 00, 00, 00, 04, 00, 05, 00, 06, 00, 00, 00, 00, 00, 00, 00, 00],
   [00, 07, 00, 08, 00, 09, 00, 10, 00, 11, 00, 12, 00, 13, 00, 14, 00, 15, 00, 16, 00],
   [00, 00, 17, 00, 18, 00, 19, 00, 20, 00, 21, 00, 22, 00, 23, 00, 24, 00, 25, 00, 00],
   [00, 00, 00, 26, 00, 27, 00, 28, 00, 29, 00, 30, 00, 31, 00, 32, 00, 33, 00, 00, 00],
   [00, 00, 00, 00, 34, 00, 35, 00, 36, 00, 37, 00, 38, 00, 39, 00, 40, 00, 00, 00, 00],
   [00, 00, 00, 41, 00, 42, 00, 43, 00, 44, 00, 45, 00, 46, 00, 47, 00, 48, 00, 00, 00],
   [00, 00, 49, 00, 50, 00, 51, 00, 52, 00, 53, 00, 54, 00, 55, 00, 56, 00, 57, 00, 00],
   [00, 58, 00, 59, 00, 60, 00, 61, 00, 62, 00, 63, 00, 64, 00, 65, 00, 66, 00, 67, 00],
   [00, 00, 00, 00, 00, 00, 00, 00, 68, 00, 69, 00, 70, 00, 00, 00, 00, 00, 00, 00, 00],
   [00, 00, 00, 00, 00, 00, 00, 00, 00, 71, 00, 72, 00, 00, 00, 00, 00, 00, 00, 00, 00],
   [00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 73, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00]]

threePlayerFirst :: [Int]
threePlayerFirst = [1, 2, 3, 4, 5, 6]

threePlayerSecond :: [Int]
threePlayerSecond = [41, 49, 50, 58, 59, 60]

threePlayerThrid :: [Int]
threePlayerThrid = [48, 56, 57, 65, 66, 67]