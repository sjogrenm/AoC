import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map

import Util

input1_min = 183564
input1_max = 657474

increasing (a:b:cs) = a <= b && increasing (b:cs)
increasing _ = True

has2adj (a:b:cs) = a == b || has2adj (b:cs)
has2adj _ = False

has2adj' ds = any (==2) [ length g | g <- group ds ]

valid num = increasing ds && has2adj ds
  where ds = digits num

valid2 num = increasing ds && has2adj' ds
  where ds = digits num

sol1 mi ma = length [ 1 | num <- [mi..ma], valid num ]

sol2 mi ma = length [ 1 | num <- [mi..ma], valid2 num ]

main = do 
          print (sol1 input1_min input1_max)
          print (sol2 input1_min input1_max)
