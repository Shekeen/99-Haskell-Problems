dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropNextEvery xs n n

dropNextEvery :: [a] -> Int -> Int -> [a]
dropNextEvery [] k n = []
dropNextEvery (x:xs) 1 n = dropNextEvery xs n n
dropNextEvery (x:xs) k n = x : dropNextEvery xs (k-1) n
