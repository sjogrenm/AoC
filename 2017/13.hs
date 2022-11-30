import Text.Parsec

parseInput :: IO [(Int,Int)]
parseInput = do i <- readFile "13input.txt"
                return [ (read a, read b)
                       | xs <- lines i, let Right [a,b] = parse f "" xs ]
  where f = sepBy (many1 digit) (char ':' >> spaces)

sample = [(0, 3), (1, 2), (4, 4), (6, 4)]

pos t s | t' < s     = t'
        | t' < s'    = s' - t' - 1
  where s' = 2*s - 1
        t' = t `mod` (s'-1)

sol d i = [ l*s | (l,s) <- i, pos (l+d) s == 0 ]

sol1' i = sum (sol 0 i)

sol1 = do i <- parseInput
          return (sol1' i)

sol2' i = head [ d | d <- [0..], null (sol d i) ]

hm i = [ (d, sol d i) | d <- [0..] ]

sol2 = do i <- parseInput
          return (sol2' i)

main = do s <- sol2
          print s

stateAt d i = [ (l, s, pos d s) | (l,s) <- i ]

{-
pos 0 3 = 0
pos 1 3 = 1
pos 2 3 = 2
pos 3 3 = 1

pos 4 3 = 0
pos 5 3 = 1
pos 6 3 = 2
pos 7 3 = 1

pos 8 3 = 0
pos 9 3 = 1


pos 0 4 = 0
pos 1 4 = 1
pos 2 4 = 2
pos 3 4 = 3
pos 4 4 = 2
pos 5 4 = 1

pos 6 4 = 0
pos 7 4 = 1
pos 8 4 = 2


pos 4 5 = 4
pos 5 5 = 3
pos 6 5 = 2
pos 7 5 = 1
-}
