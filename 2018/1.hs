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

input1 = parseInput r "1.txt"
  where r :: String -> Int
        r ('+':xs) = read xs
        r xs = read xs

sol1 xs = sum xs


sol2 xs = sol2' (cycle xs) 0 Set.empty
  where sol2' (x:xs) curr seen
          | curr `Set.member` seen      = curr
          | otherwise                   = sol2' xs (curr+x) (curr `Set.insert` seen)

main = do i <- input1
          print (sol1 i)
          print (sol2 i)
