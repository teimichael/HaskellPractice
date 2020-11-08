module Tree
    ( postorder
    , inorder
    , outFunc
    ) where

data Tree = Leaf | Node Tree Int Tree

tree :: Tree
tree = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf))

postorder :: Tree -> [Int]
postorder Leaf = []
postorder (Node l x r) = postorder l ++ postorder r ++ [x]

inorder :: Tree -> [Int]
inorder Leaf = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r

outFunc :: IO ()
outFunc = putStrLn $ show $ inorder tree