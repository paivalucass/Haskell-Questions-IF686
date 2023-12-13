btoi :: String -> Int
btoi [] = 0
btoi (x:xs) | x == '0' = btoi xs
 | otherwise = 2 ^ length xs + btoi xs 

main = do
    s <- getLine
    let result = btoi s
    print result