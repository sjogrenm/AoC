import Data.Bits
import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace, traceShow)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as Map

import Util

ex, input :: [Int]
ex = [0,3,6]
input = [16,11,15,0,1,7]

iter1 visited lastNum turn = lastNum : iter1 visited' newNum (turn + 1)
  where visited' = Map.insert lastNum turn visited
        newNum = case Map.lookup lastNum visited of
                   Just t -> turn - t
                   Nothing -> 0

iter2 visited lastNum turn exp | turn == exp = lastNum
iter2 visited lastNum turn exp = iter2 visited' newNum (turn + 1) exp
  where visited' = Map.insert lastNum turn visited
        newNum = case Map.lookup lastNum visited of
                   Just t -> turn - t
                   Nothing -> 0


sol is = (init is ++ iter1 initial (last is) (length is))
  where initial = Map.fromList (zip (init is) [1..])

sol' is exp = iter2 initial (last is) (length is) exp
  where initial = Map.fromList (zip (init is) [1..])

sol1 is = sol is !! 2019

sol2 is = sol is !! (30000000 - 1)

main = do --i <- input1
          --print input
          --print (sol1 input)
          print (sol' input 2020)
          print (sol' input 30000000)
          --print (sol2 input)