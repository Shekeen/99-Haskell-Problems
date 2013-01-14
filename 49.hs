gray 1 = ["0", "1"]
gray n = map (\ x -> '0' : x) (gray (n-1)) ++ map (\ x -> '1' : x) (reverse (gray (n-1)))