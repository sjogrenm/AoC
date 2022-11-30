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
input1 = do [x] <- parseInput f "7.txt"
            return x
  where f = map read . splitOn ','

sol1 xs = minimum [ fuel1 k xs | k <- [minimum xs .. maximum xs] ]

fuel1 k xs = sum [ abs (k - x) | x <- xs ]

sol2 xs = minimum [ fuel2 k xs | k <- [minimum xs .. maximum xs] ]

-- 
fuel2 k xs = sum [ (a * (a+1)) `div` 2 | x <- xs, let a = abs (k - x) ]


main = do xs <- input1
          --print xs
          print (sol1 xs)
          print (sol2 xs)
