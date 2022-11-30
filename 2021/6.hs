import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import Util

input1 :: IO [Int]
input1 = do [x] <- parseInput f "6.txt"
            return x
  where f = map read . splitOn ','

sol1 xs = sol1' 80 xs
sol1' 0 xs = length xs
sol1' n xs = sol1' (n-1) $ step xs
step [] = []
step (0:xs) = 6 : 8 : step xs
step (x:xs) = (x-1) : step xs

sol2 xs = sol2' 256 (mkState xs (mkArray $ replicate 9 0))

mkState [] st = elems st
mkState (x:xs) st = mkState xs (accum (+) st [(x,1)])

sol2' 0 st = sum st
sol2' n st = sol2' (n-1) $ step2 st

step2 [a0,a1,a2,a3,a4,a5,a6,a7,a8] = [a1,a2,a3,a4,a5,a6,a7+a0,a8,a0]

{-



-}

main = do ls <- input1
          --print $ sol2' 80 (mkState ls (mkArray $ replicate 9 0))
          --print ls
          --print (sol1 [3,4,3,1,2])
          --print (sol1 ls)
          print (sol2 ls)