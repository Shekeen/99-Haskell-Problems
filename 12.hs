data REElem a = Single a | Multiple Int a
	deriving (Show)
	
decodeModified :: [REElem a] -> [a]
decodeModified [] = []
decodeModified ((Single x) : xs) = [x] ++ decodeModified xs
decodeModified ((Multiple n a) : xs) = (replicate n a) ++ decodeModified xs