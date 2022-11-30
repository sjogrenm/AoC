import Data.Graph

parseInput = do i <- readFile "12input.txt"
                return (map parseLine $ lines i)

parseLine :: String -> (Int, Int, [Int])
parseLine xs = case words (filter (/=',') xs) of
                 (n1:"<->":ns) -> (read n1,read n1,map read ns)

sample1 = [(0, 0, [2]), (1, 1, [1]), (2, 2, [0,3,4]), (3,3,[2,4]), (4,4,[2,3,6]), (5,5,[6]), (6,6,[4,5])]

fscc i = map flattenSCC . stronglyConnComp $ i

sol1' i = let foo = fscc i
           in length $ head (filter (0`elem`) foo)

sol1 = do i <- parseInput
          return (sol1' i)

sol2' i = let foo = fscc i
           in length foo

sol2 = do i <- parseInput
          return (sol2' i)
