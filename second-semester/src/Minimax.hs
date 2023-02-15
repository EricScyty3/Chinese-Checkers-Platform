module Minimax where
-- the module of the shallow minimax search: Paranoid and BRS
import GameTree
import Board

type MGameTreeStatus = (PlayerIndex, -- current player of the turn
                        Board, -- the board state delivered from the parent
                        [[Pos]], -- the list of positions of the internal board for each player
                        Int -- the total players
                        )
