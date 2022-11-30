import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map

import Util

ex1 = [16,10,15,5,1,11,7,19,6,12,4]

ex2 = [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]

input1 :: IO [Int]
input1 = parseInput read "10.txt"

deltas [x] = [3]
deltas (x1:x2:xs) = x2-x1 : deltas (x2:xs)

sol1 xs = length a * length b
  where ys = sort xs
        ds = head ys : deltas ys
        [a,b] = group $ sort ds

sol2 xs = choices ds
  where ys = sort xs
        ds = head ys : deltas ys

num1pairs (1:1:xs) = 1 + num1pairs (1:xs)
num1pairs (_:xs) = num1pairs xs
num1pairs _ = 0

choices (1:1:1:1:1:xs) = error "dang it"
choices (1:1:1:1:xs) = 7 * choices xs
choices (1:1:1:xs) = 4 * choices xs
choices (1:1:xs) = 2 * choices xs
choices (_:xs) = choices xs
choices [] = 1

main = do i <- input1
          --print i
          print (sol1 i)
          print (sol2 i)

{-

 1,4,5,6,7,10,11,12,15,16,19
1,3,1,1,1,3, 1, 1, 3, 1, 3, 3


1,4 - 5 - 6 - 7 - 10
      5 - 7 - 10
    - 6 - 7 - 10
    - 7 - 10

1 1 1 3
1 1 3
1 3
1 3


 1,2,3,4,7,8,9,10,11,14
1,1,1,1,3,1,1,1, 1, 3, 3


1 - 2 - 3 - 4 - 7
1 - 2 - 4 - 7
1 - 3 - 4 - 7
1 - 4 - 7
2 - 3 - 4 - 7
2 - 4 - 7
3 - 4 - 7

1 1 1 1 3
1 1 1 3
1 1 1 3
1 1 1 3
1 1 3
1 1 3
1 1 3


1,1,1,3     -> 4
1,1,1,1,3   -> 7



-}