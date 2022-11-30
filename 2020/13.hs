import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace, traceShow)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map

import Util

currTime = 1002576
inputString = "13,x,x,x,x,x,x,37,x,x,x,x,x,449,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,773,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17"
ls1 :: [Int]
ls1 = [ read x | x <- splitOn ',' inputString, x /= "x" ]

ls2 :: [(Int,Int)]
ls2 = [ (read x, i) | (x,i) <- zip (splitOn ',' inputString) [0..], x /= "x" ]

sol1 = sol1' currTime ls1

sol1' currTime ls = let (l,t) = minimumBy (comparing snd) [ (l, f l 1) | l <- ls ]
                     in l * (t - currTime)
  where f l k | l*k >= currTime     = l*k
              | otherwise           = f l (k+1)

sol2 = sol2' ls2 7692307692307

sol2' ls k | all ok ({-traceShow k-} ls) = t
  where t = let (l,0) = head ls in l*k
        ok (a,b) = let (q,r) = (t+b) `quotRem` a in r == 0
sol2' ls k = sol2' ls (k+1)

ex2 = [(17,0), (13,2), (19,3)]

sol2'' ls = sum [ k*r*snd3 (gcd2 k b) | (b,o) <- ls, let r = b - (o `mod` b), let k = foldr1 (lcm) [ b2 | (b2,_) <- ls, b /= b2 ] ] `mod` lc
  where lc = foldr1 (lcm) (map fst ls)

gcd2' a 0 x y r s = (a,x,y)
gcd2' a b x y r s = gcd2' b c x' y' r' s'
  where (q,c) = a `quotRem` b
        x' = r
        y' = s
        r' = x - q * r
        s' = y - q * s

gcd2 a b = gcd2' a b 1 0 0 1

main = do --i <- input1
          print ls1
          print ls2
          print sol2
          --print (sol1 i ((0,0), (1,0)))
          --print (sol2 i ((0,0), (10,-1)))


{-

(a,b)  (c,d)  (e,f) ...

a*x + b = c*y + d = e*z + f ...


13x = 37y+7

x = -119, y = -42

-37 = (-2)*13 + (-11)
13 = (-1)*(-11) + 2
-11 = (-5)*2 + (-1)

-1 = -11 - (-5)*2 = -11 - (-5)*(13 - (-1)*(-11)) = 6*(-11) - (-5)*13 = 6*(-37 - (-2)*13) - (-5)*13 = 6*(-37) + 17*13

-}