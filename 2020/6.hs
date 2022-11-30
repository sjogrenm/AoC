import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import Util

input1 = do contents <- readFile "6.txt"
            let xs = [ concatMap words y | y <- splitOn "" (lines contents) ]
            return xs

sol1 xs = sum [ length y | x <- xs, let y = foldr1 union x ]

sol2 xs = sum [ length y | x <- xs, let y = foldr1 intersect x ]

main = do i <- input1
          --print i
          print (sol1 i)
          print (sol2 i)
