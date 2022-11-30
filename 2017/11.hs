parseInput = do i <- readFile "11input.txt"
                return (splitOn ',' $ concat $ lines i)

splitOn _ "" = []
splitOn c xs = case span (/=c) xs of
                 (ys,[])   -> [ys]
                 (ys,_:zs) -> ys : splitOn c zs

move1 :: (Int,Int) -> String -> (Int,Int)
move1 (nwse,ns) dir
  = case dir of
      "nw" -> (nwse-1,ns)
      "se" -> (nwse+1,ns)
      "n" -> (nwse,ns-1)
      "s" -> (nwse,ns+1)
      "ne" -> (nwse+1,ns-1)
      "sw" -> (nwse-1,ns+1)

move :: [String] -> (Int,Int)
move = foldl move1 (0,0)

dist (a,b) = maximum [abs a, abs b, abs (a+b)]

sol1 = do i <- parseInput
          let pos = move i
          return (dist pos)

move2 (pos,maxdist) dir = (pos', max maxdist (dist pos'))
  where pos' = move1 pos dir

sol2 = do i <- parseInput
          let (pos,maxdist) = foldl move2 ((0,0),0) i
          return maxdist
