module Projection2 where

-- through modifying the x-y coordinates 
{-
        (1,1)
        (6,9) (7,8) (8,7) (9,6)
        (5,8) (6,7) (7,6) (8,5)
        (4,7) (5,6) (6,5) (7,4)
(1,-1)  (3,6) (4,5) (5,4) (6,3)
-}

type Pos2 = (Int, Int)

projectGreen' :: Pos2 -> Pos2
projectGreen' (x, y) = let disY = ((x - ix) + (y - iy)) `div` 2
                           (ox, oy) = (ix + disY, iy + disY) -- move Y
                       in  (x - ox, disY)
    where
        (ix, iy) = (3, 6)

reverseGreen' :: Pos2 -> Pos2
reverseGreen' (x,y) = let (ox, oy) = (ix + x, iy - x) -- move x
                     in  (ox + y, oy + y)            -- move y
    where
        (ix, iy) = (3, 6)
{-
         (2, 0)
         (12,9) (11,8) (10,7) (9, 6)
         (10,9) (9, 8) (8, 7) (7, 6)
         (8, 9) (7, 8) (6, 7) (5, 6)
(-1,-1)  (6, 9) (5, 8) (4, 7) (3, 6)
-}
projectBlue' :: Pos2 -> Pos2
projectBlue' (x, y) = let disX = iy - y
                          (ox, oy) = (ix - disX, iy - disX) -- move x
                      in  (disX, (x - ox) `div` 2)
    where
        (ix, iy) = (6, 9)

reverseBlue' :: Pos2 -> Pos2
reverseBlue' (x, y) = let (ox, oy) = (ix - x, iy - x) -- move x
                     in  (2*y + ox, oy)              -- move y
    where
        (ix, iy) = (6, 9)
{-
        (1,-1)
        (15,6) (13,6) (11,6) (9, 6)
        (14,7) (12,7) (10,7) (8, 7)
        (13,8) (11,8) (9, 8) (7, 8)
(-2,0)  (12,9) (10,9) (8, 9) (6, 9)
-}
projectPurple' :: Pos2 -> Pos2
projectPurple' (x, y) = let disY = iy - y
                            (ox, oy) = (ix + disY, iy - disY) -- move y
                        in  ((ox-x) `div` 2, disY)
    where
        (ix, iy) = (12, 9)

reversePurple' :: Pos2 -> Pos2
reversePurple' (x, y) = let (ox, oy) = (ix - 2*x, iy) -- move x
                       in  (ox + y, oy - y)          -- move y
    where
        (ix, iy) = (12, 9)
{-
        (-1,-1)
        (12,3) (11,4) (10,5) (9, 6)
        (13,4) (12,5) (11,6) (10,7)
        (14,5) (13,6) (12,7) (11,8)
(-1,1)  (15,6) (14,7) (13,8) (12,9)
-}
projectRed' :: Pos2 -> Pos2
projectRed' (x, y) = let disY = ((ix - x) + (iy - y)) `div` 2
                         (ox, oy) = (ix - disY, iy - disY) -- move y
                     in  (ox - x, disY)
    where
        (ix, iy) = (15, 6)

reverseRed' :: Pos2 -> Pos2
reverseRed' (x, y) = let (ox, oy) = (ix - x, iy + x) -- move x
                    in  (ox - y, oy - y)            -- move y
    where
        (ix, iy) = (15, 6)
{-
        (-2,0)
        (6, 3) (7, 4) (8, 5) (9, 6)
        (8, 3) (9, 4) (10,5) (11,6)
        (10,3) (11,4) (12,5) (13,6)
(1, 1)  (12,3) (13,4) (14,5) (15,6)
-}
projectOrange' :: Pos2 -> Pos2
projectOrange' (x, y) = let disX = y - iy
                            (ox, oy) = (ix + disX, iy + disX) -- move x 
                        in  (disX, (ox - x) `div` 2)
    where
        (ix, iy) = (12, 3)

reverseOrange' :: Pos2 -> Pos2
reverseOrange' (x, y) = let (ox, oy) = (ix + x, iy + x) -- move x
                       in  (ox - 2*y, oy)              -- move y
    where
        (ix, iy) = (12, 3)
{-
        (-1,1)
        (3, 6) (5, 6) (7, 6) (9, 6)
        (4, 5) (6, 5) (8, 5) (10,5)
        (5, 4) (7, 4) (9, 4) (11,4)
(2, 0)  (6, 3) (8, 3) (10,3) (12,3)
-}
projectBlack' :: Pos2 -> Pos2
projectBlack' (x, y) = let disY = y - iy
                           (ox, oy) = (ix - disY, iy + disY) -- move Y
                       in  ((x-ox) `div` 2, disY)
    where
        (ix, iy) = (6, 3)

reverseBlack' :: Pos2 -> Pos2
reverseBlack' (x, y) = let (ox, oy) = (ix + 2 * x, iy) -- move x
                      in  (ox - y, oy + y)            -- move y
    where
        (ix, iy) = (6, 3)
