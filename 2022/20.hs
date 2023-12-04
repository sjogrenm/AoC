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

input :: IO [Int]
input = parseInput read "20.txt"

ex = [1,2,-3,3,-2,0,4]

-- list is reverse rxs ++ [x] ++ xs
moveRight 0 rxs x xs = reverse rxs ++ [x] ++ xs
moveRight k rxs x xs | k > length xs    

-- 5 [b,a] c [d,e]
-- 3 [e,d,b,a] c []

-- [1,2,-3,3,-2,0,4]
-- [0,1, 2,3, 4,5,6]

-- []