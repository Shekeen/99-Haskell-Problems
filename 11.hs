data REElem a = Single a | Multiple Int a
  deriving (Show)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : (pack $ dropWhile (==x) xs)

convert :: [a] -> (Int, a)
convert xs = (length xs, head xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map convert . pack

compact :: (Int, a) -> REElem a
compact (1, a) = Single a
compact (n, a) = Multiple n a

encodeModified :: (Eq a) => [a] -> [REElem a]
encodeModified = map compact . encode
