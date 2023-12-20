data Tree t = Node t (Tree t) (Tree t) | Nilt
              deriving (Read, Show)


split :: String -> [String]
split [] = []
split s = take 8 s : split (drop 8 s)

decode :: Tree Int -> String
decode Nilt  = [] 
decode (Node y esq dir) | y `mod` 5 == 0 = decode esq ++ "E" ++ decode dir 
                        | y `mod` 5 == 1 = decode esq ++ "M" ++ decode dir 
                        | y `mod` 5 == 2 = decode esq ++ "A" ++ decode dir 
                        | y `mod` 5 == 3 = decode esq ++ "C" ++ decode dir 
                        | y `mod` 5 == 4 = decode esq ++ "S" ++ decode dir 

dna :: Tree Int -> [String]
dna Nilt = []
dna x = split (decode x)


main :: IO ()
main = do

  input <- getLine

  let result = dna (read input :: Tree Int)

  print result