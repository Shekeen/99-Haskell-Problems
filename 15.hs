repli :: [a] -> Int -> [a]
repli [] n = []
repli (x:xs) n = concat [first, second]
    where
        first = mult x n
        second = repli xs n

mult :: a -> Int -> [a]
mult x 0 = []
mult x n = x : mult x (n-1)
