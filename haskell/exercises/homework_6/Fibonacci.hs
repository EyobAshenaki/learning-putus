module Fibonacci where

-- ********** Exercise 1 **********

fib :: Integer -> Integer
fib n
  | n < 1 = -1
  | otherwise = case n of
      1 -> 0
      2 -> 1
      n -> fib (n - 2) + fib (n - 1)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [1 ..]]

-- ********** Exercise 2 **********

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- ********** Exercise 3 **********

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show :: Show a => Stream a -> String
  show = show . take 200 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- ********** Exercise 4 **********

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

-- ********** Exercise 5 **********

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))

-- ruler :: Stream Integer
-- ruler = streamMap powOf2 (streamFromSeed (+ 1) 1)

-- powOf2 :: Integer -> Integer
-- powOf2 x
--   | odd x = 0
--   | otherwise = 1 + powOf2 (x `div` 2)

ruler :: Stream Integer
ruler = interleaveStreams ruler (streamRepeat 0)

fibonacciFunction :: Stream Integer
fibonacciFunction = streamMap fst $ streamFromSeed (\(prev, cur) -> (cur, prev + cur)) (0, 1)

nthFibonacciFunction :: Integer -> Integer
nthFibonacciFunction n = round ((1 / sqrt 5) * ((((1 + sqrt 5) / 2) ** fromIntegral n) - (((1 - sqrt 5) / 2) ** fromIntegral n)))

fibFun :: Stream Integer
fibFun = streamMap nthFibonacciFunction $ nats