range :: Int -> Int -> [Int]
range n m
  | n == m = [n]
  | n < m  = n : range (n+1) m
  | n > m  = n : range (n-1) m
