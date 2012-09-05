slice :: [a] -> Int -> Int -> [a]
slice xs first last = drop (first - 1) (take last xs) 
