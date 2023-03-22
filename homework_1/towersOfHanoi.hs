type Peg = String

type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [(Move, Int)]
hanoi n start end temp
  | n == 0 = []
  | n > 0 = hanoi (n - 1) start temp end ++ [((start, end), n)] ++ hanoi (n - 1) temp end start
