somarMultiplos :: [Int] -> Int -> [Int]
somarMultiplos list 0 = [0 | x <- list]
somarMultiplos [] m = []
somarMultiplos (x:xs) m | m > x = 0 : somarMultiplos xs m
                        | otherwise = sum [0, m..x] : somarMultiplos xs m

main = do
    lista <- getLine
    let readList = read lista :: [Int]
    num <- getLine
    let readNum = read num :: Int
    let result = somarMultiplos readList readNum
    print result