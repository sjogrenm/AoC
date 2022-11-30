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

input1 = parseInput r "1.txt"
  where r :: String -> Int
        r xs = read xs

sol1 (x:xs) = sol1' x xs 0
sol1' _ [] n = n
sol1' x (y:ys) n = sol1' y ys (n + if y > x then 1 else 0)


sol2 xs = sol1 (slide xs)
slide (a:b:c:ds) = (a+b+c) : slide (b:c:ds)
slide _ = []


main = do i <- input1
          print (sol1 i)
          print (sol2 i)
