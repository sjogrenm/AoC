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

input1 = parseInput id "3.txt"

sol sX sY [] _ = 0
sol sX sY xs@(y:_) k = sol sX sY (drop sY xs) (k+sX) + n
  where n | y !! k == '#'       = 1
          | otherwise           = 0

sol1 xs = sol 3 1 xs 0

sol2 xs = product [sol 1 1 xs 0, sol 3 1 xs 0, sol 5 1 xs 0, sol 7 1 xs 0, sol 1 2 xs 0]

main = do i <- input1
          --print i
          let i' = map cycle i
          print (sol1 i')
          print (sol2 i')
