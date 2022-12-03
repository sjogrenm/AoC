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

input = parseInput r "2.txt"
  where r :: String -> (Char,Char)
        r [a,b,c] = (a,c)

score1 (a,c) = shape c + win a c
  where shape 'X' = 1
        shape 'Y' = 2
        shape 'Z' = 3
        win 'A' 'X' = 3
        win 'A' 'Y' = 6
        win 'A' 'Z' = 0
        win 'B' 'X' = 0
        win 'B' 'Y' = 3
        win 'B' 'Z' = 6
        win 'C' 'X' = 6
        win 'C' 'Y' = 0
        win 'C' 'Z' = 3

sol1 [] = 0
sol1 (x:xs) = score1 x + sol1 xs


score2 (a,c) = win c + shape a c
  where win 'X' = 0
        win 'Y' = 3
        win 'Z' = 6
        shape 'A' 'X' = 3   -- rock -> scissors
        shape 'A' 'Y' = 1   -- rock -> rock
        shape 'A' 'Z' = 2   -- rock -> paper
        shape 'B' 'X' = 1
        shape 'B' 'Y' = 2
        shape 'B' 'Z' = 3
        shape 'C' 'X' = 2
        shape 'C' 'Y' = 3
        shape 'C' 'Z' = 1

sol2 [] = 0
sol2 (x:xs) = score2 x + sol2 xs

-- sol2 xs = sol1 (slide xs)
-- slide (a:b:c:ds) = (a+b+c) : slide (b:c:ds)
-- slide _ = []


main = do i <- input
        --   print (head i)
          print (sol1 i)
          print (sol2 i)
