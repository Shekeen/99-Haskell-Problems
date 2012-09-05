split :: [a] -> Int -> ([a], [a])
split xs n = (first, second)
    where
        first = take n xs
        second = drop n xs

rotate :: [a] -> Int -> [a]
rotate xs n
    | n > len   = rotate xs (n - len)
    | n < 0     = rotate xs (n + len)
    | otherwise = let (f, s) = splitAt n xs in s ++ f
    where len = length xs
