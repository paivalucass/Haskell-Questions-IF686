-- Questão 1

rlencode0 :: [Int] -> [Int]
rlencode0 [] = []
rlencode0 (x:xs) = encode (x:xs) 0
 where 
  encode [] z | z /= 0 = [0,z]
  encode [] _ = []
  encode (y:ys) z | y == 0 = encode ys (z+1)
                  | y /= 0 && z /= 0 = 0 : z : y : encode ys 0
                  | otherwise = y : encode ys 0
                 
count :: Int -> [Int]  
count 0 = []
count x = 0 : count (x-1)                

rldecode0 :: [Int] -> [Int]
rldecode0 [] = []
rldecode0 x = decode x 0
 where 
    decode [] z = []
    decode (y:ys) z | y /= 0 && z == 0 = y : decode ys z
                    | y /= 0 && z == 1 = count y ++ decode ys 0
                    | y == 0 = decode ys 1

                
-- Questão 2
encode :: String -> Int -> String
encode [] _ = []
encode [y] z | z /= 0 = y : [toEnum (z+48)] 
             | z == 0 = [y]
encode (y:ys) z | y == head ys = encode ys (z+1)
                | y /= head ys && z == 0 = y : encode ys 0
                | y /= head ys && z /= 0 = y : toEnum (z + 48+1) : encode ys 0  

rlencodeLetras :: String -> String
rlencodeLetras [] = []
rlencodeLetras x = encode x 0


