{-# LANGUAGE TupleSections #-}

module BoardState_test where

import State
import Board

getPiece :: Pos -> SB Board BoardType
getPiece pos = S $ \board -> (getElement board pos,board)

getPiecePos :: BoardType -> SB Board Pos
getPiecePos b = S(getPos b, )

getPieceColour :: BoardType -> SB Board (Maybe Colour)
getPieceColour b = S(getColour b,)

getPieceIndex :: BoardType -> SB sb Int
getPieceIndex b = S(getIndex b,)

isPieceOccupied :: BoardType -> SB sb (Maybe Bool)
isPieceOccupied b = S(isOccupied b,)

repaintPiece :: Colour -> BoardType -> SB Board BoardType
repaintPiece c b = S(repaint c b, )

erasePiece :: BoardType -> SB Board BoardType
erasePiece b = S(erase b, )

replacePiece :: BoardType -> SB Board ()
replacePiece b = do s <- stState
                    stUpdate (newBoard s)
    where
        newBoard board = let (x, y) = getPos b
                             newRow = replace x b (board !! y)
                         in  replace y newRow board

repaintPiecePath :: BoardType -> BoardType -> SB Board ()
repaintPiecePath s e = do sc <- getPieceColour s
                          case sc of
                            Nothing -> return () -- just do nothing
                            Just c  -> do ns <- erasePiece s
                                          replacePiece ns
                                          ne <- repaintPiece c e
                                          replacePiece ne

-- erasePiecesInBoard :: Bool -> [Colour] -> SB Board ()
-- erasePiecesInBoard t cs = do s <- stState
--                              stUpdate $ eraseBoard t cs s 

-- adjacentJumpList :: BoardType -> SB Board [BoardType]
-- adjacentJumpList b = do s <- stState 
--                         return (findAvaliableNeighbors s b)

-- chainedJumpList :: BoardType -> SB Board [BoardType]
-- chainedJumpList b = do s <- stState
--                        return (searchWithoutLooping s [] b)

-- -- check if this can be reached by one jump
-- -- One adjacent jump range
-- -- search for all neighbor positions around that are not occupied
-- findAvaliableNeighbors :: Board -> BoardType -> [BoardType]
-- findAvaliableNeighbors myBoard b = filter (isJustFalse . isOccupied) (findTureNeighbors (findValidNeighbors (getPos b) myBoard))

-- -- findTureNeighbors :: Pos -> Board -> Bool
-- findTureNeighbors :: [BoardType] -> [BoardType]
-- findTureNeighbors = filter (isJust . isOccupied)
-- -- search for all piece positions inside the board around
-- findValidNeighbors :: Pos -> Board -> [BoardType]
-- findValidNeighbors (x, y) myBoard = map (getElement myBoard) (filter testValidPos [(x-1, y-1), (x-2, y), (x-1, y+1), (x+1, y+1), (x+2, y), (x+1, y-1)])
-- -- test if a piece's position is out of boarder
testValidPos2 :: Pos -> SB Board Bool
testValidPos2 (x, y) = return $ (x >= 0 && y >= 0) && (x <= boardWidth - 1) && (y <= boardHeight - 1)

-- findValidNeighborPieces :: (Int, Int) -> SB Board [BoardType]
-- findValidNeighborPieces pos = do s <- stState
--                                  map getPiece (filter testValidPos2 surroundings pos)
                                 --filter (isJustFalse . isOccupied) newList


--     where
--         surroundings (x,y) = [(x-1, y-1), (x-2, y), (x-1, y+1), (x+1, y+1), (x+2, y), (x+1, y-1)]
        


-- test = do testValidPieces (9, 0)

-- myPrint (x, s) = x
