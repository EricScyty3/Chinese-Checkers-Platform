
module NProjection where

-- Below is the coordinate conversion of internal board state of each colour and the external display board state
-- mainly mathematical computation, can be ignored

type Coor = (Int, Int)

-- the projected positions of the end and start based within the occupied board
goalBase :: [Coor]
goalBase = [(4,0),(5,0),(5,1),(6,0),(6,1),(6,2)]
startBase :: [Coor]
startBase = [(0,4),(0,5),(0,6),(1,5),(1,6),(2,6)]

-- [0,0,0,0,1,1,1]
-- [0,0,0,0,0,1,1]
-- [0,0,0,0,0,0,1]
-- [0,0,0,0,0,0,0]
-- [0,0,0,0,0,0,0]
-- [0,0,0,0,0,0,0]
-- [0,0,0,0,0,0,0]

-- testProjection bs = let ls = map fst bs
--                         new1 = map projectPurple ls
--                         new2 = map (switch . projectPurple' . switch) ls
--                         old1 = map reversePurple new1
--                         old2 = map (switch . reversePurple' . switch) new2
--                     in  (new1 == new2) && (old1 == old2)

--     where 
--         switch (a, b) = (b, a)

-- through modifying the x-y coordinates 
{-
         (-1,1)
         (0,9) -> (0,6)
         (3,6) (4,7) (5,8) (6,9)
         (4,5) (5,6) (6,7) (7,8)
         (5,4) (6,5) (7,6) (8,7)
  (1,1)  (6,3) (7,4) (8,5) (9,6) … （12,9) -> (6,0)
         (0,0) (1,0)
-}
projectGreen :: Coor -> Coor
projectGreen (x, y) = let disX = ((x - ix) + (y - iy)) `div` 2
                          y' = iy + disX -- move x
                          disY = y - y'
                      in  (disX, disY)
    where
        (ix, iy) = (6, 3)

reverseGreen :: Coor -> Coor
reverseGreen (x,y) = let (ox, oy) = (ix + x, iy + x) -- move x
                     in  (ox - y, oy + y)            -- move y
    where
        (ix, iy) = (6, 3)
{-
         (-1,-1)
         (3,0) -> (0,6)
         (6,3) (6,5) (6,7) (6,9)
         (7,4) (7,6) (7,8) (7,10)
         (8,5) (8,7) (8,9) (8,11)
   (0,2) (9,6) (9,8) (9,10),(9,12) (9,18) -> (6,0)
-}
projectBlue :: Coor -> Coor
projectBlue (x, y) = let disY = ix - x
                         y' = iy - disY
                         disX = (y - y') `div` 2
                     in  (disX, disY)
    where
        (ix, iy) = (9, 6)

reverseBlue :: Coor -> Coor
reverseBlue (x, y) = let (ox, oy) = (ix, iy + 2*x) -- move x
                     in  (ox - y, oy - y)         -- move y
    where
        (ix, iy) = (9, 6)
{-
           (0,-2)
           (9,0) -> (0,6)
           (9,6) (8,7) (7,8) (6,9)
           (9,8) (8,9) (7,10), (6,11)
           (9,10) (8,11) (7,12) (6,13) -> (3,1)
    (-1,1) (9,12) (8,13) (7,14) (6,15) -> (3,0)
-}

projectPurple :: Coor -> Coor
projectPurple (x, y) = let disX = ix - x
                           y' = iy + disX -- move x
                           disY = (y' - y) `div` 2
                       in  (disX, disY)
    where
        (ix, iy) = (9, 12)

reversePurple :: Coor -> Coor
reversePurple (x, y) = let (ox, oy) = (ix - x,iy + x) -- move x
                       in  (ox, oy - 2*y)              -- move y
    where
        (ix, iy) = (9, 12)

{-
(1,-1)
(9,12) (8,11) (7,10) (6,9)
(8,13) (7,12) (6,11) (5,10)
(7,14) (6,13) (5,12) (4,11)
(6,15) (5,14) (4,13) (3,12) (-1,-1)
-}
projectRed :: Coor -> Coor
projectRed (x, y) = let disX = ((ix - x) + (iy - y)) `div` 2
                        y' = iy - disX
                        disY = y'- y
                    in  (disX, disY)
    where
        (ix, iy) = (6, 15)

reverseRed :: Coor -> Coor
reverseRed (x, y) = let (ox, oy) = (ix - x, iy - x) -- move x
                    in  (ox + y, oy - y)            -- move y
    where
        (ix, iy) = (6, 15)
{-
(1,1)
(6,15) (6,13) (6,11) (6,9) 
(5,14) (5,12) (5,10) (5,8)
(4,13) (4,11) (4,9) (4,7)
(3,12) (3,10) (3,8) (3,6) (0,-2)
-}

projectOrange :: Coor -> Coor
projectOrange (x, y) = let disY = x - ix
                           y' = iy + disY-- move x 
                           disX = (y'- y) `div` 2
                       in  (disX, disY)
    where
        (ix, iy) = (3, 12)

reverseOrange :: Coor -> Coor
reverseOrange (x, y) = let (ox, oy) = (ix, iy - 2*x)    -- move x
                       in  (ox + y, oy + y)             -- move y
    where
        (ix, iy) = (3, 12)
{-
(0,2)
(3,12) (4,11) (5,10) (6,9)
(3,10) (4,9) (5,8) (6,7)
(3,8) (4,7) (5,6) (6,5)
(3,6) (4,5) (5,4) (6,3) (1,-1)
-}

projectBlack :: Coor -> Coor
projectBlack (x, y) = let disX = x - ix
                          y' = iy - disX -- move Y
                          disY = (y - y') `div` 2
                      in  (disX, disY)
    where
        (ix, iy) = (3, 6)

reverseBlack :: Coor -> Coor
reverseBlack (x, y) = let (ox, oy) = (ix + x, iy - x) -- move x
                      in  (ox, oy + 2*y)               -- move y
    where
        (ix, iy) = (3, 6)