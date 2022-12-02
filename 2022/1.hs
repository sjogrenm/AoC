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

processInput :: String -> [[Int]]
processInput s = [ [ read x | x <- ys ] | ys <- splitOn [] ls ]
  where ls = lines s

input1 = do i <- readFile "1.txt"
            return (processInput i)

sol1 xs = maximum [ sum y | y <- xs ]

sol2 xs = sum $ take 3 $ reverse (sort [ sum y | y <- xs ])

main = do i <- input1
          print (sol1 i)
          print (sol2 i)
