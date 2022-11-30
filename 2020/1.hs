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

sol1 xs = head [ x * y | x <- xs, y <- xs, x + y == 2020 ]


sol2 xs = head [ x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020 ]


main = do i <- input1
          print (sol1 i)
          print (sol2 i)
