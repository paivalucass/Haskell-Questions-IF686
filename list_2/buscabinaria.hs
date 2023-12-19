data Tree t = Nilt |
              Node Int (Tree t) (Tree t)
              deriving (Read)

verif :: Tree t -> Int -> Int -> Bool 
verif Nilt x y = True
verif (Node n esq dir) t v | n < t && v == 0 = True
                           | n > t && v == 0 = False
                           | n > t && v == 1 = True 
                           | n < t && v == 1 = False

bigger :: Tree t -> Int -> Bool
bigger Nilt x = True 
bigger (Node n esq dir) t | t < n = False   
                          | otherwise = bigger dir t 

lower :: Tree t -> Int -> Bool
lower Nilt x = True 
lower (Node n esq dir) t | t > n = False   
                         | otherwise = lower esq t 

isBST :: Ord t => Tree t -> Bool
isBST Nilt = True
isBST (Node n esq dir) = verif esq n 0 && verif dir n 1 && bigger esq n && lower dir n && isBST esq && isBST dir

main = do
       s <- getLine
       let result = isBST (read s::Tree Int)
       print result
