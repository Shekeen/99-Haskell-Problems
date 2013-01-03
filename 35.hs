primes = [n | n<-[2..], not $ elem n [j*k | j<-[2..n-1], k<-[2..min j (n`div`j)]]]

primeFactors 1 = []
primeFactors n = primeFactors' n [] primes
  where
    primeFactors' 1 ps primes' = reverse ps
    primeFactors' n ps primes'
      | n `mod` (primes' !! 0) == 0 = primeFactors' (n `div` (primes' !! 0)) ((primes' !! 0) : ps) primes'
      | otherwise = primeFactors' n ps (tail primes')