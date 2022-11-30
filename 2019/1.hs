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

f x = max (x `div` 3 - 2) 0

sol1 = sum . map f


sol2 xs = sum [ sum (sol2' (f x)) | x <- xs ]
  where sol2' 0 = []
        sol2' x = x : sol2' (f x)


main = do i <- input1
          print (sol1 i)
          print (sol2 i)
