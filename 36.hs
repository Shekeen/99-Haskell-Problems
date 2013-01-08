primes = [n | n<-[2..], not $ elem n [j*k | j<-[2..n-1], k<-[2..min j (n`div`j)]]]

primeFactors 1 = []
primeFactors n = primeFactors' n [] primes
  where
    primeFactors' 1 ps primes' = reverse ps
    primeFactors' n ps primes'
      | n `mod` (primes' !! 0) == 0 = primeFactors' (n `div` (primes' !! 0)) ((primes' !! 0) : ps) primes'
      | otherwise = primeFactors' n ps (tail primes')

myPack :: Eq a => [a] -> [[a]]
myPack [] = []
myPack (x:xs) = (x:first) : myPack rest
  where
    getReps [] = ([], [])
    getReps (y:ys)
      | y == x  = let (f, r) = getReps ys in (y:f, r)
      | otherwise = ([], (y:ys))
    (first, rest) = getReps xs

encode_groups [] = []
encode_groups (x:xs) = (encode_group x) : (encode_groups xs)
  where
    encode_group x = (head x, length x)

myEncode = encode_groups . myPack
      
primeFactorsMult = myEncode . primeFactors