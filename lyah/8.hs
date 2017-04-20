module Tree
  (singleton
  ,insertTree
  ,fromList)
where

data Tree a
  = EmptyTree
  | Node {value::a, left::Tree a, right::Tree a}
  deriving (Show, Eq)

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)
  
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x EmptyTree
  = singleton x
insertTree x tree@(Node a left right)
  | x == a = tree
  | x < a = Node a (insertTree x left) right
  | x > a = Node a left (insertTree x right)

fromList :: (Ord a) => [a] -> Tree a
fromList = foldl (flip insertTree) EmptyTree

