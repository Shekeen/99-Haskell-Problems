myPack :: Eq a => [a] -> [[a]]
myPack [] = []
myPack (x:xs) = (x:first) : myPack rest
    where
        getReps [] = ([], [])
        getReps (y:ys)
            | y == x  = let (f, r) = getReps ys in (y:f, r)
            | otherwise = ([], (y:ys))
        (first, rest) = getReps xs

encode_group :: Eq x => [x] -> (Int, x)
encode_group [] = error "Wrong parameter"
encode_group lst = (length lst, head lst)

encode_groups :: Eq x => [[x]] -> [(Int, x)]
encode_groups [] = []
encode_groups (x:xs) = (encode_group x) : (encode_groups xs)

myEncode :: Eq x => [x] -> [(Int, x)]
myEncode = encode_groups . myPack
