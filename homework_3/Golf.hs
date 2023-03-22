module Golf where

-- skips takes a list and returns a list of lists where each list is every nth element of the original list
skips :: [a] -> [[a]]
skips xs = map (everyNth xs) [1 .. length xs]

-- everyNth takes a list and an integer n and returns a list of every nth element of the list
everyNth :: [a] -> Int -> [a]
everyNth xs n = map fst $ filter (\(x, i) -> i `mod` n == 0) $ zip xs [1 ..]

-- localMaxima takes a list of integers and returns a list of all the local maxima in the input list
localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : xs)
  | y > x && y > z = y : localMaxima (y : z : xs)
  | otherwise = localMaxima (y : z : xs)
localMaxima _ = []
