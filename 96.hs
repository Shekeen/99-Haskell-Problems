import Data.Char

data State = S1 | S2 | S3

automata :: State -> String -> Bool
automata S1 [] = False
automata S1 (l:ls) = if isAlpha l
                     then automata S2 ls
                     else False
automata S2 [] = True
automata S2 (l:ls)
         | l == '-' = automata S3 ls
         | isAlphaNum l = automata S2 ls
         | otherwise = False
automata S3 [] = False
automata S3 (l:ls) = if isAlphaNum l
                     then automata S2 ls
                     else False

identifier :: String -> Bool
identifier str = automata S1 str
