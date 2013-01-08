primes = [n | n<-[2..], not $ elem n [j*k | j<-[2..n-1], k<-[2..min j (n`div`j)]]]

isPrime 1 = False
isPrime n = (primesN !! ((length primesN) - 1)) == n
  where
    primesN = takeWhile (<= n) primes 
