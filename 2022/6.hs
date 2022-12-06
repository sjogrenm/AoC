import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import Util

input = readFile "6.txt"

sol1 xs = sol 4 xs

f l n xs (z:zs) | Set.size s == l       = n
                | otherwise             = f l (n+1) (tail xs++[z]) zs
  where s = Set.fromList xs

sol l xs = let (ys,zs) = splitAt l xs
            in f l l ys zs

sol2 xs = sol 14 xs

main = do i <- input
        --   print (head i)
          print (sol1 i)
          print (sol2 i)
