module FoldTree where

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h

foldTree :: [a] -> Tree a
foldTree xs = foldr insertTree Leaf xs where
  insertTree x Leaf = Node 0 Leaf x Leaf
  insertTree x (Node n Leaf a Leaf) = Node (n+1) (insertTree x Leaf) a Leaf
  insertTree x (Node n ln a rn)
      | lh > rh = Node n ln a (insertTree x rn)
      | lh < rh = Node n nt a rn
      | lh == rh = Node (nh+1) nt a rn
      where
        lh = treeHeight ln
        rh = treeHeight rn
        nt = insertTree x ln
        nh = treeHeight nt
