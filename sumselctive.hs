sumNumbers :: String -> Int 
sumNumbers [] = 0
sumNumbers (x:xs) | ((fromEnum x - 48) < 10) && ((fromEnum x - 48) > 0) = (fromEnum x - 48) + sumNumbers xs
 | otherwise = sumNumbers xs 

main = do
  a <- getLine
  let result = sumNumbers a
  print result