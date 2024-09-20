module Golf where

import Data.List

-- ********** skips **********

-- skips takes a list and returns a list of lists where each list is every nth element of the original list
skips :: [a] -> [[a]]
skips xs = map (everyNth xs) [1 .. length xs]

-- everyNth takes a list and an integer n and returns a list of every nth element of the list
everyNth :: [a] -> Int -> [a]
everyNth xs n = map fst $ filter (\(x, i) -> i `mod` n == 0) $ zip xs [1 ..]

-- ********** localMaxima **********

-- localMaxima takes a list of integers and returns a list of all the local maxima in the input list
localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : xs)
  | y > x && y > z = y : localMaxima (y : z : xs)
  | otherwise = localMaxima (y : z : xs)
localMaxima _ = []

-- ********** histogram **********

groupInKind :: [Integer] -> [[String]]
groupInKind xs = group $ map show $ sort xs

-- add empty list at in the list of list for elements that does not exist in the list
addEmptyList :: [Integer] -> [[String]] -> [[String]]
-- addEmptyList xs ys = map (\x -> if x `elem` map head ys then searchList x ys else []) $ map show xs
addEmptyList xs ys = map ((\x -> if x `elem` map head ys then searchList x ys else []) . show) xs

-- search for a list that has the same head as the string
searchList :: String -> [[String]] -> [String]
searchList s ys = head $ filter (\x -> head x == s) ys

populateLists :: [Integer] -> [[String]]
populateLists xs = map (\x -> spaces (maxLength - x) ++ stars x) $ getListLength xs
  where
    getListLength xs = map length $ addEmptyList [0 .. 9] $ groupInKind xs
    maxLength = maximum $ map length $ groupInKind xs
    spaces m = replicate m " "
    stars n = replicate n "*"

labelHistogram :: [[String]] -> [String]
labelHistogram xs = map concat xs ++ ["==========", "0123456789"]

histogram :: [Integer] -> String
histogram xs = unlines $ labelHistogram $ transpose $ populateLists xs
