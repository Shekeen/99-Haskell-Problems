elementAt :: [a] -> Int -> a
elementAt lst n
    | n == 1    = head lst
    | n < 1     = error "Out of bounds index"
    | otherwise = elementAt (tail lst) (n-1)

