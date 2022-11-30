import Data.List

parseInput = do contents <- readFile "4input.txt"
                return [ words l | l <- lines contents ]

countValid xs = length [ 1 | x <- xs, length x == length (nub x) ]

sol1 = do i <- parseInput
          return $ countValid i

countValid2 xs = length [ 1 | x <- xs, length x == length (nubBy f x) ]
  where f a b = sort a == sort b

sol2 = do i <- parseInput
          return $ countValid2 i
