import qualified Data.Set as S
import qualified Data.Map as M
import Data.List

input :: [Int]
input = map read $ words "10 3 15 10 5 15 5 15 9 2 5 8 5 2 3 6"

hashCode :: [Int] -> Integer
hashCode xs = hashCode' xs (toInteger 1)

hashCode' [] c = c
hashCode' (x:xs) c = hashCode' xs (c*100 + toInteger x)

permute xs = let (b,i) = maximumBy cmp (zip xs [0..])
                 (ys,_:zs) = splitAt i xs
                 (b',zs') = moveBlock (b,zs)
                 (0,result) = until ((==0).fst) moveBlock (b',ys++[0]++zs')
              in result
  where cmp (x,i) (y,j) = case compare x y of
                            EQ -> compare j i
                            r -> r

moveBlock (c,x:xs) | c > 0 = let (c',xs') = moveBlock (c-1,xs)
                              in (c',x+1:xs')
moveBlock (c,xs) = (c,xs)



sol1' visited xs n
  | c `S.member` visited  = n
  | otherwise           = sol1' (c `S.insert` visited) xs' (n+1)
  where c = hashCode xs
        xs' = permute xs

sol1 = sol1' S.empty input 0

sol2' visited xs n = case c `M.lookup` visited of
                       Just m -> n - m
                       Nothing -> sol2' (M.insert c n visited) xs' (n+1)
  where c = hashCode xs
        xs' = permute xs

sol2 = sol2' M.empty input 0
