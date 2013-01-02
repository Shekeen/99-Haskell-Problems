lsort [] = []
lsort (l:ls) = lsort (filter (\ x -> (length x) < (length l)) ls) ++ [l] ++ lsort (filter (\ x -> (length x) >= (length l)) ls)