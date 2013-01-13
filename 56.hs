data Tree a = Empty
            | Branch a (Tree a) (Tree a)
     deriving (Show, Eq)

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror _ Empty = False
mirror Empty _ = False
mirror (Branch _ a b) (Branch _ c d) = and [mirror a d, mirror b c]

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ a b) = mirror a b