split :: Char -> String -> [String]
split _ [] = []
split c s = takeWhile (/= c) s : split c (dropWhile (== c) (dropWhile (/= c) s))

minMax :: [Double] -> (Double,Double) -> (Double, Double)
minMax [] (min,max) = (min,max)
minMax (x:xs) (min,max) | (min, max) == (1000000.0, -1) = minMax xs (x, x)
                        | x > max = minMax xs (min,x)
                        | x < min = minMax xs (x,max)
                        | otherwise = minMax xs (min,max)

values :: [String] -> [Double]
values [] = []
values (x:xs) | fromEnum(last x) - 48 < 10 && fromEnum(last x) - 48 >= 0 = read x : values xs
              | otherwise = values xs 


minMaxCartao :: String -> (Double, Double)
minMaxCartao [] = (0, 0)
minMaxCartao string = minMax(values(split ';' string)) (100000.0, -1)
main = do
    a <- getLine
    let result = minMaxCartao a
    print result