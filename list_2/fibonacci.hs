
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | x `mod` 2 == 0 = x : pares xs
             | otherwise = pares xs

fibonacci :: Int -> [Int]
fibonacci 0 = []
fibonacci n = oi 0 1 n
    where
        oi a b n | n == 0 = []
                 | otherwise = a : oi b (a+b) (n-1)

coder :: String -> String
coder [] = []
coder x = x ++ show(sum(pares(fibonacci(length x))))

main = do
    a <- getLine
    let result = coder a
    print result