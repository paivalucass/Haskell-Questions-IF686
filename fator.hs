isPrime :: Int -> Bool
isPrime x | x == 1 = False
          | x == 2 = True
          | otherwise = isPrimeAux x (x-1)
          where
          isPrimeAux x y | y == 1 = True
                         | x `mod` y == 0 = False
                         | otherwise = isPrimeAux x (y-1)

fatPrimeAux :: Int -> Int -> Int -> [(Int, Int)]
fatPrimeAux x y z | x == 1 && z == 0 = []
                  | x `mod` y == 0 = fatPrimeAux (x `div` y) y (z+1)
                  | x `mod` y /= 0 && z >= 1 = (y,z) : fatPrimeAux x (y+1) 0
                  | otherwise = fatPrimeAux x (y+1) 0

fatPrime :: Int -> [(Int, Int)]
fatPrime x | x == 1 = []
           | x < 0 = []
           | x == 0 = []
           | isPrime x = [(x,1)]
           | otherwise = fatPrimeAux x 2 0

main = do
      a <- getLine
      let result = fatPrime (read a :: Int)
      print result