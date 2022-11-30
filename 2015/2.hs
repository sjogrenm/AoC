import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Debug.Trace (trace)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.Map as Map

import Util

parseLine :: String -> [Int]
parseLine = map read . splitOn 'x'

parse = parseInput parseLine "2.txt"

reqPaper xs = let [a,b,c] = sort xs
               in a*b + 2*(a*b + b*c + c*a)

solve1 ys = sum $ map reqPaper ys

reqRibbon xs = let [a,b,c] = sort xs
                in 2*a + 2*b + a*b*c

solve2 ys = sum $ map reqRibbon ys
