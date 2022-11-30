ex1 = [[5,1,9,5],[7,5,3],[2,4,6,8]]

minmax (x:xs) = minmax' xs x x
  where minmax' [] mi ma = (mi,ma)
        minmax' (x:xs) mi ma = minmax' xs (min x mi) (max x ma)

chk1 xs = let (mi,ma) = minmax xs in ma - mi

sheetChecksum f xss = sum (map f xss)

parseInput :: IO [[Int]]
parseInput = do contents <- readFile "2input.txt"
                return [ [ read w | w <- words l ] | l <- lines contents ]

sol1 = parseInput >>= (print . sheetChecksum chk1)

ex2 = [[5,9,2,8],[9,4,7,3],[3,8,6,5]]

chk2 (x:xs) = case findChk x xs of
                Just y -> y
                Nothing -> chk2 xs

findChk x [] = Nothing
findChk x (y:ys) | m1 == 0      = Just d1
                 | m2 == 0      = Just d2
                 | otherwise    = findChk x ys
  where (d1,m1) = x `divMod` y
        (d2,m2) = y `divMod` x

sol2 = parseInput >>= (print . sheetChecksum chk2)
