import Data.List

digits = ["zero", "one", "two", "three", "four",
          "five", "six", "seven", "eight", "nine"]

numToDigitList 0 = [0]
numToDigitList n
               | n < 10 = [n]
               | otherwise = numToDigitList (n `div` 10) ++ [n `mod` 10]

fullWords n = concat $ intersperse "-" [digits !! d | d <- numToDigitList n]