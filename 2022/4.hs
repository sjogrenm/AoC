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

input = parseInput r "4.txt"
  where r :: String -> ((Int,Int),(Int,Int))
        r xs = let [a,b] = splitOn ',' xs
                   [c,d] = splitOn '-' a
                   [e,f] = splitOn '-' b
                in ((read c,read d),(read e,read f))


sol1 xs = length [ () | ((c,d),(e,f)) <- xs, let a = [c..d], let b = [e..f], null (a \\ b) || null (b \\ a) ]

sol2 xs = length [ () | ((c,d),(e,f)) <- xs, let a = [c..d], let b = [e..f], notNull (a `intersect` b) ]

main = do i <- input
        --   print (head i)
          print (sol1 i)
          print (sol2 i)
