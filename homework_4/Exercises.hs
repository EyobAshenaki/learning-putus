import Data.List ((\\))

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
foldTree = foldr insert Leaf

-- checks if the tree has a leaf as an immediate child
hasLeaf :: Tree a -> Bool
hasLeaf (Node _ Leaf _ _) = True
hasLeaf (Node _ _ _ Leaf) = True
hasLeaf _ = False

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

-- inserts a value into the tree in the correct position based on the height of the left and right subtrees
insert :: Ord a => a -> Tree a -> Tree a
insert = go 0
  where
    go h x Leaf = Node h Leaf x Leaf
    go h x whole@(Node h' leftT val rightT)
      | hasLeaf whole = if leftT == Leaf then Node nextHeight (go h x leftT) val rightT else Node nextHeight leftT val (go h x rightT)
      | otherwise = if height leftT < height rightT then Node nextHeight (go h x leftT) val rightT else Node nextHeight leftT val (go h x rightT)
      where
        nextHeight = 1 + max (height leftT) (height rightT)

-- ********** Exercise 3 **********

xor :: [Bool] -> Bool
xor = odd . length . foldr (\x acc -> if x then x : acc else acc) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- ********** Exercise 4 **********

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) $ [1 .. k] \\ cartProd k
  where
    k = (n - 2) `div` 2
    cartProd n = [x + y + (2 * x * y) | x <- [1 .. n], y <- [1 .. n], x < y]