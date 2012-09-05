insertAt :: a -> [a] -> Int -> [a]
insertAt el xs 1 = el:xs
insertAt el (x:xs) n = x : insertAt el xs (n - 1)
