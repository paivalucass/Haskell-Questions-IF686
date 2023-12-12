addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos x = " " ++ addEspacos (x-1)

toright :: Int -> String -> String
toright x st = addEspacos x ++ st

parseInput str = let [n, s] = words str
                 in (read n, s)
main :: IO()
main = interact $ uncurry toright . parseInput