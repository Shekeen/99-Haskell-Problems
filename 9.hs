myPack :: Eq a => [a] -> [[a]]
myPack [] = []
myPack (x:xs) = (x:first) : myPack rest
    where
        getReps [] = ([], [])
        getReps (y:ys)
            | y == x  = let (f, r) = getReps ys in (y:f, r)
            | otherwise = ([], (y:ys))
        (first, rest) = getReps xs
