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

data Line = (Int,Int) :-> (Int,Int)
    deriving (Show,Eq)

input1 = parseInput f "5.txt"
  where f xs = let [pt1,"->",pt2] = words xs in p pt1 :-> p pt2
        p ys = let [x,y] = splitOn ',' ys in (read x, read y)

sol1 ls = sol1' ls Map.empty
sol1' [] m = length [ () | c <- Map.elems m, c > 1 ]
sol1' (((x1,y1) :-> (x2,y2)) : ls) m | x1 == x2         = sol1' ls $ foldr (Map.alter inc) m [ (x1,y) | y <- [ min y1 y2 .. max y1 y2 ] ]
                                     | y1 == y2         = sol1' ls $ foldr (Map.alter inc) m [ (x,y1) | x <- [ min x1 x2 .. max x1 x2 ] ]
                                     | otherwise        = sol1' ls m
  where inc (Just x) = Just (x+1)
        inc Nothing  = Just 1

sol2 ls = sol2' ls Map.empty
sol2' [] m = length [ () | c <- Map.elems m, c > 1 ]
sol2' (((x1,y1) :-> (x2,y2)) : ls) m | x1 == x2         = sol2' ls $ foldr (Map.alter inc) m [ (x1,y) | y <- [ min y1 y2 .. max y1 y2 ] ]
                                     | y1 == y2         = sol2' ls $ foldr (Map.alter inc) m [ (x,y1) | x <- [ min x1 x2 .. max x1 x2 ] ]
                                     | otherwise        = sol2' ls $ foldr (Map.alter inc) m $ zip (coords x1 x2) (coords y1 y2)
  where inc (Just x) = Just (x+1)
        inc Nothing  = Just 1

coords c1 c2 | c1 < c2      = [c1..c2]
             | c1 > c2      = [c1,c1-1..c2]

main = do ls <- input1
          print (sol1 ls)
          print (sol2 ls)