type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa j = calculator j 0
 where 
 calculator [] n = n
 calculator (("Multiplica",x):xs) n = calculator xs (x*n)
 calculator (("Soma",x):xs) n = calculator xs (x + n)
 calculator (("Subtrai",x):xs) n = calculator xs (n - x)
 calculator (("Divide",x):xs) n | x == 0 = -666
                                | otherwise = calculator xs (n `div` x)

main = do
    a <- getLine
    let result = executa (read a)
    print result