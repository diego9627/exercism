module  BST (bstLeft, bstRight, bstValue, singleton, insert, fromList, toList) where

import Data.List (foldl')

data BST a = Leaf | Node {bstValue :: a, left :: BST a, right :: BST a }  deriving (Show, Eq)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Leaf = Nothing
bstLeft (Node _ n _) = Just n

bstRight :: BST a -> Maybe (BST a)
bstRight Leaf = Nothing
bstRight (Node _ _ n) = Just n

singleton :: a -> BST a
singleton n = Node n Leaf Leaf

insert :: Ord a => a -> BST a -> BST a
insert n Leaf = Node n Leaf Leaf
insert n bst 
  | n <= bstValue bst = bst {left  = (insert n (left  bst))}
  | otherwise         = bst {right = (insert n (right bst))}

fromList :: Ord a => [a] -> BST a 
fromList = foldl' (flip insert) Leaf


toList :: BST a -> [a]
toList Leaf = []
toList (Node v l r) = (toList l) ++ [v] ++ (toList r)

