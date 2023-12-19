data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)


norte :: (Int,Int) -> [Command] -> (Int,Int)
norte x [] = x
norte (x,xs) (Forward y:ys) = norte (x,xs+y) ys 
norte (x,xs) (Backward y:ys) = norte (x,xs-y) ys 
norte x (TurnLeft:ys) = oeste x ys 
norte x (TurnRight:ys) = leste x ys

sul :: (Int,Int) -> [Command] -> (Int,Int)
sul x [] = x
sul (x,xs) (Forward y:ys) = sul (x,xs-y) ys 
sul (x,xs) (Backward y:ys) = sul (x,xs+y) ys 
sul x (TurnLeft:ys) = leste x ys 
sul x (TurnRight:ys) = oeste x ys

leste :: (Int,Int) -> [Command] -> (Int,Int)
leste x [] = x
leste (x,xs) (Forward y:ys) = leste (x+y,xs) ys 
leste (x,xs) (Backward y:ys) = leste (x-y,xs) ys 
leste x (TurnLeft:ys) = norte x ys 
leste x (TurnRight:ys) = sul x ys

oeste :: (Int,Int) -> [Command] -> (Int,Int)
oeste x [] = x
oeste (x,xs) (Forward y:ys) = oeste (x-y,xs) ys 
oeste (x,xs) (Backward y:ys) = oeste (x+y,xs) ys 
oeste x (TurnLeft:ys) = sul x ys 
oeste x (TurnRight:ys) = norte x ys

destination :: (Int,Int) -> [Command] -> (Int,Int) 
destination x [] = x
destination x y = norte x y

main = do
       a <- getLine
       b <- getLine
       let result = destination (read a) (read b)
       print result
