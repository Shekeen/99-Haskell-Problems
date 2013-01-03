myGCD a b
    | a > b = myGCD' a b
    | otherwise = myGCD' b a
  where
    myGCD' a 0 = a
    myGCD' a b = myGCD' b (a `mod` b)

totient 1 = 1
totient n = length $ filter (coprime n) [1..n]
  where
    coprime a b = (myGCD a b == 1)