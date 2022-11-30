import qualified Data.Map.Strict as Map

data Dir = R | L | U | D
        deriving (Eq,Show)

move (x,y) R = (x+1,y)
move (x,y) L = (x-1,y)
move (x,y) U = (x,y-1)
move (x,y) D = (x,y+1)

generate n True = replicate n R ++ replicate n U ++ generate (n+1) False
generate n False = replicate n L ++ replicate n D ++ generate (n+1) True


positions = genPositions (0,0) (generate 1 True)
  where genPositions p (m:ms) = p : genPositions (move p m) ms

distForPos k = abs x + abs y
  where (x,y) = positions !! (k-1)

input = 289326

sol1 = distForPos input


mapping d = genMapping (Map.singleton (0,0) 1) (drop 1 positions)
  where genMapping :: Map.Map (Int,Int) Int -> [(Int,Int)] -> Int
        genMapping m ((x,y):ps)
          | sumValues > d       = sumValues
          | otherwise           = genMapping (Map.insert (x,y) sumValues m) ps
          where values = [ Map.lookup (x+a,y+b) m | a <- [-1..1], b <- [-1..1] ]
                sumValues = sum [ x | Just x <- values ]

sol2 = mapping input
