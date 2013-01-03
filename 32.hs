myGCD a b
      | a > b = myGCD' a b
      | otherwise = myGCD' b a
  where
    myGCD' a 0 = a
    myGCD' a b = myGCD' b (a `mod` b)
