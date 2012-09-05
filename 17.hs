split :: [a] -> Int -> ([a], [a])
split xs n = (first, second)
    where
        first = take n xs
        second = drop n xs
