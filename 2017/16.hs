import Data.Char
import Data.List
import Data.List.Split
import Data.Array.Unboxed

parseInput = do i <- readFile "16input.txt"
                return $ parse i

data Instr = Spin Int | Exchg Int Int | Partner Char Char
        deriving Show

parse i = map f $ splitOn "," (concat $ lines i)
  where f ('s':xs) = Spin (read xs)
        f ('x':xs) = let [a,b] = splitOn "/" xs
                      in Exchg (read a) (read b)
        f ('p':a:'/':b:[]) = Partner a b

startPos = ['a'..'p']

eval pos (Spin n) = let (xs,ys) = splitAt (length pos - n) pos
                     in ys ++ xs
eval pos (Exchg a b) = let (xs,y:ys) = splitAt c pos
                           (zs,w:ws) = splitAt (d-c-1) ys
                        in xs ++ [w] ++ zs ++ [y] ++ ws
  where c = min a b
        d = max a b
eval pos (Partner a b) = let (xs,y:ys) = span (`notElem` [a,b]) pos
                             (zs,w:ws) = span (`notElem` [a,b]) ys
                          in xs ++ [w] ++ zs ++ [y] ++ ws

sol1' pos is = foldl' eval pos is

sol1 = do is <- parseInput
          return (sol1' startPos is)

sol2' pos is 0 = pos
sol2' pos is n = sol2' (sol1' pos is) is (n-1)

sol2 = do is <- parseInput
          return (sol2' startPos is (1000*1000*1000))

main = do p <- sol2
          print p



exchange x y z
  | x == z = y
  | y == z = x
  | otherwise = z


permuteNames :: String -> [Instr] -> UArray Char Char
permuteNames xs is = foldl' f initial [ (x,y) | Partner x y <- is ]
  where initial = listArray (head xs, last xs) xs
        f arr (x,y) = amap (exchange x y) arr


permuteIndexes :: Int -> [Instr] -> UArray Int Int
permuteIndexes sz is = foldl' f initial is
  where initial = listArray (0, sz-1) [0..]
        f arr (Spin n)    = ixmap (bounds arr) (\i -> (i - n) `mod` sz) arr
        f arr (Exchg i j) = ixmap (bounds arr) (exchange i j) arr
        f arr _ = arr

mulA :: (Ix i, IArray a i) => a i i -> a i i -> a i i
p1 `mulA` p2 = ixmap (bounds p1) (p2 !) p1

expA :: (Ix i, IArray a i) => a i i -> Int -> a i i
p `expA` 1 = p
p `expA` n = case n `divMod` 2 of
               (h, 0) -> (p `mulA` p) `expA` h
               (h, 1) -> ((p `mulA` p) `expA` h) `mulA` p

sol16b n xs is = let ns = permuteNames xs is `expA` n
                     ix = permuteIndexes (length xs) is `expA` n
                  in map (elems ns !!) (elems ix)

sol16a = sol16b 1
