data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

              isBST :: Ord t => Tree t -> Bool
              
main = do
       s <- getLine
       let result = isBST (read s::Tree Int)
       print result