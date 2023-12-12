find ::  Char -> [(Char,Char)] -> Char
find x (z:zs) | x == fst z = snd z
              | otherwise = find x zs

decEnigma :: String -> [(Char, Char)] -> String
decEnigma (x:xs) [] = []
decEnigma [] [] = []
decEnigma [] (y:ys) = []
decEnigma (x:xs) (y:ys) = find x (y:ys) : decEnigma xs (y:ys)

main = do
    a <- getLine
    b <- getLine
    let result = decEnigma a (read b)
    print result