import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Debug.Trace (trace)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import Util

input1 = parseInput id "2.txt"


sol1 xs = length (filter has2 xs') * length (filter has3 xs')
  where xs' = map (group . sort) xs

has2 xs = any (\g -> length g == 2) xs
has3 xs = any (\g -> length g == 3) xs



sol2 xs@(x:_) = filter (\y -> length y == len) (commons xs)
  where len = length x - 1

common [] [] = []
common (a:as) (b:bs) | a == b    = a : common as bs
                     | otherwise = common as bs

commons [] = []
commons (x:xs) = map (common x) xs ++ commons xs



main = do i <- input1
          print (sol1 i)
          print (sol2 i)
