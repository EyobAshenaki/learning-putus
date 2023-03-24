-- ********** Exercise 1 **********

fun1 :: [Integer] -> Integer
fun1 = product . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- ********** Exercise 2 **********

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: Ord a => Eq a => [a] -> Tree a
foldTree = foldr insert' Leaf

-- checks if the tree has a leaf as an immediate child
hasLeaf :: Tree a -> Bool
hasLeaf (Node _ Leaf _ _) = True
hasLeaf (Node _ _ _ Leaf) = True
hasLeaf _ = False

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x wholeT@(Node h leftT val rightT)
  | hasLeaf wholeT = if leftT == Leaf then Node (h + 1) (insert x leftT) val rightT else Node (h + 1) leftT val (insert x rightT)
  | otherwise = if height leftT < height rightT then Node h (insert x leftT) val rightT else Node h leftT val (insert x rightT)

-- inserts a value into the tree in the correct position based on the height of the left and right subtrees
insert' :: Ord a => a -> Tree a -> Tree a
insert' = go 0
  where
    go :: Ord a => Integer -> a -> Tree a -> Tree a
    go h x Leaf = Node h Leaf x Leaf
    go h x whole@(Node h' leftT val rightT)
      | hasLeaf whole = if leftT == Leaf then Node (h' + 1) (go h x leftT) val rightT else Node (h' + 1) leftT val (go h x rightT)
      | otherwise = if height leftT < height rightT then Node (max h h + 1) (go h x leftT) val rightT else Node (max h h + 1) leftT val (go h x rightT)
