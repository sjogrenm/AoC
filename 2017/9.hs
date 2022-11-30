parseInput = readFile "9input.txt"

filterGarbage ('<':ys) = filterGarbage (dropGarbage ys)
filterGarbage (y:ys)   = y : filterGarbage ys
filterGarbage []       = []

countGarbage ('<':ys) = let (as,'>':bs) = span (/='>') ys
                         in length as + countGarbage bs
countGarbage (y:ys)   = countGarbage ys
countGarbage []       = 0

unbang ('!':x:xs) = unbang xs
unbang (y:ys) = y : unbang ys
unbang [] = []

dropGarbage xs = let '>':ys = dropWhile (/='>') xs in ys

score curr [] = []
score curr ('{':xs) = curr : score (curr+1) xs
score curr ('}':xs) = score (curr-1) xs
score curr (_:xs) = score curr xs

sol1' xs = sum (score 1 (filterGarbage $ unbang xs))

sol1 = do i <- parseInput
          return (sol1' i)

sol2' xs = countGarbage (unbang xs)

sol2 = do i <- parseInput
          return (sol2' i)
