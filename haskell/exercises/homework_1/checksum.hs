toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x : y : xs) = x : y * 2 : doubleEveryOther' xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs)
  | x < 10 = x + sumDigits xs
  | otherwise = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = result `mod` 10 == 0
  where
    result = sumDigits $ doubleEveryOther $ toDigits n