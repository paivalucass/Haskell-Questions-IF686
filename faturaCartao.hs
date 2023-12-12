
split :: Char -> String -> [String]
split _ [] = []
split c s = takeWhile (/= c) s : split c (dropWhile (== c) (dropWhile (/= c) s))

values :: [String] -> String -> [Double]
values [] _ = []
values (x:xs) m | drop 3 x == m = read(head(drop 2 (x:xs))) : values (drop 3 (x:xs)) m
                | otherwise = values (drop 3 (x:xs)) m
    
logMes :: String -> String -> Double
logMes [] _ = 0.00
logMes m string = foldl (+) 0 (values (split ';' string) m)

main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result