table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- [True, False], b <- [True, False]]