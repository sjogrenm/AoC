import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Debug.Trace (trace)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import Util

getInput = parseInput (parse . words) "3.txt"

data Foo = Foo Int (Int, Int) (Int, Int)
        deriving (Eq, Ord, Show)

parse ['#':id, _, offset, size] = Foo (read id) (ox, oy) (sx, sy)
  where [ox, oy] = map read $ splitOn ',' (init offset)
        [sx, sy] = map read $ splitOn 'x' size


sol1 result = V.length (V.filter (\xs -> length xs > 1) result)

fill a [] = return a
fill a (Foo id (ox,oy) (sx,sy) : xs) = do
        let ps = [ px*1000 + py | px <- [ox,ox+1..ox+sx-1], py <- [oy,oy+1..oy+sy-1] ]
        mapM_ (MV.modify a (id:)) ps
        fill a xs


sol2 ids result = foldl (foldr Set.delete) ids overlaps
  where overlaps = V.filter (\xs -> length xs > 1) result



main = do i <- getInput
          let result :: V.Vector [Int]
              result = V.create $ do 
                  a <- MV.replicate (1001*1000) []
                  fill a i
          print (sol1 result)
          print (sol2 (Set.fromList [ id | Foo id _ _ <- i ]) result)
