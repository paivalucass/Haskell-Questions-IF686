data Ops = SUM | MUL | SUB
           deriving (Read,Eq)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)


evalTree :: IntTree -> Int
evalTree (Nilt x) = x
evalTree (Node x esq dir) | x == SUM = evalTree(esq) + evalTree (dir)
                          | x == SUB = evalTree(esq) - evalTree (dir)
                          | x == MUL = evalTree(esq) * evalTree (dir)


main = do
    s <- getLine
    let result = evalTree (read s)
    print result