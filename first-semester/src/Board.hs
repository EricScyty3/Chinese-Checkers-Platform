module Board where

data BoardType = R | B | G | E | U deriving (Eq, Show)
type RowBoardState = [BoardType]

boardWidth :: Int
boardWidth = 21
boardHeight :: Int
boardHeight = 13

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


