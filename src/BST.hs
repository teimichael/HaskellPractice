module BST
    ( outFunc
    ) where

data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show

-- member :: (Eq a) => a -> Tree a -> Bool
-- member v Nil = False
-- member v (Node l y r)
--     | v == y = True
--     | otherwise = member v l || member v r
member :: Ord a => a -> Tree a -> Bool
member _ Nil = False
member v (Node l x r)
      | v == x = True
      | v < x = member v l
      | v > x = member v r


insert :: (Ord a) => a -> Tree a -> Tree a
insert v Nil = Node Nil v Nil
insert v (Node l x r)
      | v == x = error "Duplicate key"
      | v < x = Node (insert v l) x r
      | v > x = Node l x (insert v r)


delete :: Ord a => a -> Tree a -> Tree a
delete _ Nil = Nil
delete v (Node l x r)
    | v < x = Node (delete v l) x r
    | v > x = Node l x (delete v r)

-- Node to be deleted is leaf: Simply remove from the tree. 
delete _ (Node Nil _ Nil) = Nil
--  Node to be deleted has only one child: Copy the child to the node and delete the child 
delete _ (Node Nil _ r) = r
delete _ (Node l _ Nil) = l
-- Node to be deleted has two children: Find inorder successor of the node. Copy contents of the inorder successor (rightmost minimum) to the node and delete the inorder successor.
delete _ (Node l _ r) = Node l rm r'
    where   rm = rightMin r
            r' = delete rm r

-- Inorder predecessor version
-- delete _ (Node l _ r) = Node l' lm r
--     where   lm = leftMax l
--             l' = delete lm l


rightMin :: Tree a -> a
rightMin (Node Nil rm _) = rm
rightMin (Node l _ _) = rightMin l

leftMax :: Tree a -> a
leftMax (Node _ lm Nil) = lm
leftMax (Node _ _ r) = leftMax r


inorder :: Tree a -> [a]
inorder Nil = []
inorder (Node l x r) = inorder l ++ x : inorder r

depth :: Tree a -> Int
depth Nil = 0
depth (Node l x r) = max (depth' l 1) (depth' r 1)
    where   depth' Nil acc = acc
            depth' (Node l x r) acc = max (depth' l (acc + 1)) (depth' r (acc + 1))


tree :: Tree Int
tree = Node (Node (Node Nil 0 Nil) 1 (Node (Node (Node Nil 2 Nil) 3 (Node Nil 4 Nil)) 5 Nil)) 6 (Node (Node Nil 7 Nil) 8 (Node Nil 9 Nil))

outFunc :: IO ()
outFunc = putStrLn $ show $ delete 6 tree