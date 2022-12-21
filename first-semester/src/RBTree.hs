module RBTree where

-- the node index of the Reb Black tree, representing a board state
type Key = Int
-- the score of the represented board state for lookup table or the history heuristic
-- the colour of the node, either red or black 
data TColour = Red | Black deriving (Eq, Show)
-- the Red Black tree contains the colour of node, the node value, left subtree, node index, right rubtree  
data RBTree a = RBLeaf | RBNode TColour a (RBTree a) Key (RBTree a) deriving (Eq, Show)

-- repaints a tree's root node and returns 
repaint :: TColour -> RBTree a -> RBTree a
repaint _ RBLeaf = RBLeaf
repaint c (RBNode _ n t1 x t2) = RBNode c n t1 x t2

-- returns the colour of the root of a tree
getColour :: RBTree a -> TColour
getColour RBLeaf = Black -- the leaf's colour is black 
getColour (RBNode c _ _ _ _) = c

-- returns the stored content of a node
getContent :: RBTree a -> Maybe a
getContent RBLeaf = Nothing
getContent (RBNode _ n _ _ _) = Just n

-- returns the integer index representing a board state
getKey :: RBTree a -> Maybe Key
getKey RBLeaf = Nothing
getKey (RBNode _ _ _ i _) = Just i

-- searches a node with certain index in the tree
-- returns the node's content if found, otherwise, nothing
rbSearch :: Key -> RBTree a -> Maybe a
rbSearch h RBLeaf = Nothing
rbSearch h (RBNode _ v t1 x t2)
    | h > x = rbSearch h t2
    | h < x = rbSearch h t1
    | otherwise = Just v

-- balances a tree when there are 2 consecutive red nodes 
-- parameters: root node colour, left subtree, root node index, root node value, right subtree
-- returns a weakly Red Black tree that is balanced, later the top root will be repainted to black
balance :: TColour -> RBTree a -> Key -> a -> RBTree a -> RBTree a
-- left  
balance Black (RBNode Red n2 (RBNode Red n1 t1 x1 t2) x2 t3) x3 n3 t4 = RBNode Red n2 (RBNode Black n1 t1 x1 t2) x2 (RBNode Black n3 t3 x3 t4)
balance Black (RBNode Red n1 t1 x1 (RBNode Red n2 t2 x2 t3)) x3 n3 t4 = RBNode Red n2 (RBNode Black n1 t1 x1 t2) x2 (RBNode Black n3 t3 x3 t4)
-- right
balance Black t1 x1 n1 (RBNode Red n2 t2 x2 (RBNode Red n3 t3 x3 t4)) = RBNode Red n2 (RBNode Black n1 t1 x1 t2) x2 (RBNode Black n3 t3 x3 t4)
balance Black t1 x1 n1 (RBNode Red n3 (RBNode Red n2 t2 x2 t3) x3 t4) = RBNode Red n2 (RBNode Black n1 t1 x1 t2) x2 (RBNode Black n3 t3 x3 t4)
-- otherwise, do nothing
balance c t1 x n t2 = RBNode c n t1 x t2

-- inserts a Hash and its board value into the Red Black tree while maintaining tree's properties 
rbInsert :: Key -> a -> RBTree a -> RBTree a
rbInsert h n tree = repaint Black (ins h n tree) -- recursively calling balance function may make the root node red, repaints to make it black  
    where
        -- inserts a node with red into the tree, if already exists then do something/nothing 
        -- since inserting a red node may break the tree's properties, reblances the branch after each insertion 
        -- returns a weakly Red Black tree
        ins :: Key -> a -> RBTree a -> RBTree a
        ins h n RBLeaf = RBNode Red n RBLeaf h RBLeaf
        ins h n t@(RBNode c v t1 r t2)
            | h < r = balance c (ins h n t1) r v t2
            | h > r = balance c t1 r v (ins h n t2)
            | otherwise = RBNode c n t1 r t2 -- t
            -- if the insertion's index is duplicated, would replace the old stored value with new content
            -- can be treated as an update operator for the tree


-- rebalances a tree when the black-height of the left side is one less than the right side
-- parameters: colour of the root node, left subtree, root index, root value, right subtree
-- returns a weakly Red Black tree
balL :: TColour -> RBTree a -> Key -> a -> RBTree a -> RBTree a
balL _ (RBNode Red n1 t1 x1 t2) x2 n2 t3 = RBNode Red n2 (RBNode Black n1 t1 x1 t2) x2 t3
balL _ t1 x1 n1 (RBNode Black n2 t2 x2 t3) = balance Black t1 x1 n1 (RBNode Red n2 t2 x2 t3)
balL _ t1 x1 n1 (RBNode Red n3 (RBNode Black n2 t2 x2 t3) x3 t4) = RBNode Red n2 (RBNode Black n1 t1 x1 t2) x2 (balance Black t3 x3 n3 (repaint Red t4))
-- otherwise, no change is made
balL c t1 x n t2 = RBNode c n t1 x t2

-- rebalances a tree when the black-height of the right side is one less than the left side 
-- parameters: colour of the root node, left subtree, root index, root value, right subtree
-- returns a weakly Red Black tree
balR :: TColour -> RBTree a -> Key -> a -> RBTree a -> RBTree a
balR _ t1 x1 n1 (RBNode Red n2 t2 x2 t3) = RBNode Red n1 t1 x1 (RBNode Black n2 t2 x2 t3)
balR _ (RBNode Black n1 t1 x1 t2) x2 n2 t3 = balance Black (RBNode Red n1 t1 x1 t2) x2 n2 t3
balR _ (RBNode Red n1 t1 x1 (RBNode Black n2 t2 x2 t3)) x3 n3 t4 = RBNode Red n2 (balance Black (repaint Red t1) x1 n1 t2) x2 (RBNode Black n3 t3 x3 t4)
-- otherwise, no change is made
balR c t1 x n t2 = RBNode c n t1 x t2

-- deletes a node with certain Hash value from the tree
rbDelete :: Key -> RBTree a -> RBTree a
rbDelete x t = repaint Black (del x t) -- repaints the root as the returned tree is a weakly Red Black 

-- deletes a node from a tree, if not found then do nothing 
del :: Key -> RBTree a -> RBTree a
del h RBLeaf = RBLeaf
del h (RBNode c n t1 r t2)
    | h < r = delL h t1 c r n t2
    | h > r = delR h t1 c r n t2
    | otherwise = fuse t1 t2 -- if found then just fuse its left and right subtrees as the root would be taken away 

-- deletes a node from the left subtree t1 where r is the root with colour c and t2 is the right subtree 
delL :: Key -> RBTree a -> TColour -> Key -> a -> RBTree a -> RBTree a
delL h t1 c r n t2 = if getColour t1 == Black then balL c (del h t1) r n t2 -- since the returned tree will have a top red node, the black height need to be rebalanced
                     else RBNode Red n (del h t1) r t2

-- deletes a node from the right subtree t2 where r is the root with colour c and t1 is the left subtree
delR :: Key -> RBTree a -> TColour -> Key -> a -> RBTree a -> RBTree a
delR h t1 c r n t2 = if getColour t2 == Black then balR c t1 r n (del h t2) -- since the returned tree will have a top red node, the black height need to be rebalanced
                     else RBNode Red n t1 r (del h t2)

-- gathers two trees t1 and t2 together, known that the nodes in t1 is smaller than t2
fuse :: RBTree a -> RBTree a -> RBTree a
-- when meeting leaf, just return another non-leaf tree
fuse RBLeaf t = t
fuse t RBLeaf = t
fuse lt@(RBNode Black n1 t1 x1 t2) (RBNode Red n2 t3 x2 t4) = RBNode Red n2 (fuse lt t3) x2 t4
fuse (RBNode Red n1 t1 x1 t2) rt@(RBNode Black n2 t3 x2 t4) = RBNode Red n1 t1 x1 (fuse t2 rt)
fuse (RBNode Red n1 t1 x1 t2) (RBNode Red n2 t3 x2 t4) = if getColour s == Black then RBNode Red n1 t1 x1 (RBNode Red n2 s x2 t4)
                                                         else let RBNode Red nf s1 f s2 = s
                                                              in  RBNode Red nf (RBNode Red n1 t1 x1 s1) f (RBNode Red n2 s2 x2 t4)
                                                         where s = fuse t2 t3
fuse (RBNode Black n1 t1 x1 t2) (RBNode Black n2 t3 x2 t4) = if getColour s == Black then balL Black t1 x1 n1 (RBNode Black n2 s x2 t4)
                                                             else let RBNode Red nf s1 f s2 = s
                                                                  in  RBNode Red nf (RBNode Black n1 t1 x1 s1) f (RBNode Black n2 s2 x2 t4)
                                                             where s = fuse t2 t3