data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

maiorDiametro :: Ord t => Tree t -> Int
maiorDiametro Nilt = 0 
maiorDiametro (Node x Nilt Nilt) = 0
maiorDiametro (Node x esq dir) = max (dia(esq) + dia(dir) + 1) (max (maiorDiametro(esq)) (maiorDiametro(dir)))
 where 
    dia Nilt = 0
    dia (Node x esq dir) = max (dia(esq)) (dia(dir)) + 1


main = do
       s <- getLine
       let result = maiorDiametro (read s::Tree Int)
       print result