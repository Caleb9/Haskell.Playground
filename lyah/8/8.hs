import qualified Data.Map as Map

data Vector a
  = Vector a a a
  deriving Show

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector x1 y1 z1) `vplus` (Vector x2 y2 z2)
  = Vector (x1+x2) (y1+y2) (z1+z2)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector x y z) `vectMult` k
  = Vector (x*k) (y*k) (z*k)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector x1 y1 z1) `scalarMult` (Vector x2 y2 z2)
  = x1*x2 + y1*y2 + z1*z2

data Person
  = Person
  { firstName :: String
  , lastName :: String
  , age :: Int
  } deriving (Eq, Show, Read)

data Day
  = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

-- binary tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> (Tree a)
singleton x = Node x (EmptyTree) (EmptyTree)

treeInsert :: (Ord a) => a -> (Tree a) -> (Tree a)
treeInsert x EmptyTree = singleton x
treeInsert x tree@(Node a left right)
  | x == a = tree
  | x <  a = Node a (treeInsert x left) right
  | x >  a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> (Tree a) -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x <  a = treeElem x left
  | x >  a = treeElem x right

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"
