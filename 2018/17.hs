import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
import Data.Array
import Debug.Trace (trace, traceShow)
import qualified Data.Vector as V
--import Data.Vector ((!))
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies
import Data.Graph as G
import Data.Bits

import Util


--getInput :: IO (Int, Int, Set.Set (Int,Int))
getInput = do ii <- parseInput f "17.txt"
              let jj = [ (x,y) | (xs,ys) <- ii, x <- xs, y <- ys ]
              let maxX = maximum (map fst jj)
              let maxY = maximum (map snd jj)
              return (maxX,maxY, Set.fromList jj)
  where f ys = let [c1:'=':as, ' ':c2:'=':bs] = splitOn ',' ys
                in case (c1,c2) of
                     ('x','y') -> (parseInts as, parseInts bs)
                     ('y','x') -> (parseInts bs, parseInts as)

parseInts :: String -> [Int]
parseInts xs | '.' `notElem` xs = [read xs]
parseInts xs = [read a..read b]
  where [a,"",b] = splitOn '.' xs
