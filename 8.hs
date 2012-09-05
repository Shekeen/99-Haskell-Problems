myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (x:y:xs) 
    | x == y  = myCompress (y:xs)
    | otherwise  = x : myCompress(y:xs)
