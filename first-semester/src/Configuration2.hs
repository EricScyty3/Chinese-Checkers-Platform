module Configuration2 where
import Configuration
import Zobrist
import Board

-- this version of configuration tends to compute all possible board states (excluding mirror images) rather than deleting some sections

listAllRows :: [[Int]]
listAllRows = init (permutation 7)

-- need to check the sum of the pieces accumlated so far
listAllBoards :: [[Int]] -> Int -> [OccupiedBoard]
listAllBoards _  0 = [[]]
listAllBoards rs l = [x:xs | x <- rs, xs <- listAllBoards (remainingRows x rs) (l - 1)]
    where
        remainingRows :: [Int] -> [[Int]] -> [[Int]]
        remainingRows chosedrow rs = let pieces = sum chosedrow
                                         allowedPieces = totalPieces - pieces
                                     in  filter ((<= allowedPieces) . sum) rs

allBoards :: [OccupiedBoard]
allBoards = mirrorBoardCheck $ validBoards $ listAllBoards listAllRows 7

-- ghc Configuration2.hs -O2 -fllvm -outputdir dist
-- rm -rf dist/
-- rm Configuration2
main = print (length allBoards)