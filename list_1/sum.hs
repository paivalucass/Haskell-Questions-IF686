maquinaSomar :: [Int] -> [Int]
maquinaSomar (0:0:xs) = []
maquinaSomar [0] = []
maquinaSomar [] = []
maquinaSomar (0:xs) = maquinaSomar xs
maquinaSomar x = summer x 0
 where 
 summer [] n = [n]
 summer (0:0:xs) n = [n]
 summer [0] n = [n]
 summer (0:xs) n = n : summer xs 0
 summer (x:xs) n = summer xs (x+n)

main = do
       lista <- getLine
       print $ maquinaSomar (read lista :: [Int])