unzip' :: [(Int,Int)] -> ([Int],[Int])
unzip' [] = ([],[])
unzip' ((a,b):xs) = foldr (\(a,b) (c,d) -> (a:c,b:d)) ([a],[b]) xs



main = interact $ show . unzip' . (read :: String -> [(Int,Int)])