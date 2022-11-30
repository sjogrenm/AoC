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

input = parseInput (splitAt 7) "5.txt"

binSearch mi ma [] | mi+1 == ma   = mi

binSearch mi ma (upper:xs) | upper      = binSearch (mi + (ma-mi)`div`2) ma xs
                           | otherwise  = binSearch mi (ma - (ma-mi)`div`2) xs

toBool xs = [ x `elem` "BR" | x <- xs ]

seatIds xs = [ row * 8 + col | (rowDesc,colDesc) <- xs, let row = binSearch 0 128 (toBool rowDesc), let col = binSearch 0 8 (toBool colDesc) ]

sol1 xs = maximum (seatIds xs)

sol2 xs = f ys
  where ys = sort (seatIds xs)
        f (a:b:cs) | a+1 == b       = f (b:cs)
                   | otherwise      = a+1
main = do i <- input
          --print i
          print (sol1 i)
          print (sol2 i)
