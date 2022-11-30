import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map

import Util

ex = ["F10"
     ,"N3"
     ,"F7"
     ,"R90"
     ,"F11"]

input1 :: IO [String]
input1 = parseInput id "12.txt"

type Pos = (Int,Int)

turnL (0,-1) = (-1,0)
turnL (-1,0) = (0,1)
turnL (0,1) = (1,0)
turnL (1,0) = (0,-1)

turnL (x,y) = (y,-x)

turnR (-1,0) = (0,-1)
turnR (0,-1) = (1,0)
turnR (1,0) = (0,1)
turnR (0,1) = (-1,0)

turnR (x,y) = (-y,x)

move ('N':n) ((x, y), dir) = ((x, y - read n), dir)
move ('E':n) ((x, y), dir) = ((x + read n, y), dir)
move ('S':n) ((x, y), dir) = ((x, y + read n), dir)
move ('W':n) ((x, y), dir) = ((x - read n, y), dir)
move ('L':n) (pos, (xd,yd)) = case read n of
                                90 -> (pos, turnL (xd,yd))
                                180 -> (pos, (-xd,-yd))
                                270 -> (pos, turnL (-xd,-yd))
                                360 -> (pos, (xd,yd))
move ('R':n) (pos, (xd,yd)) = case read n of
                                90 -> (pos, turnR (xd,yd))
                                180 -> (pos, (-xd,-yd))
                                270 -> (pos, turnR (-xd,-yd))
                                360 -> (pos, (xd,yd))
move ('F':n) ((x,y),(xd,yd)) = ((x + read n * xd, y + read n * yd), (xd,yd))

sol1 [] ((x,y),_) = (x, y, abs x + abs y)
sol1 (m:ms) posdir = sol1 ms (move m posdir)


move2 ('N':n) (pos,(wx,wy)) = (pos,(wx,wy - read n))
move2 ('E':n) (pos,(wx,wy)) = (pos,(wx + read n,wy))
move2 ('S':n) (pos,(wx,wy)) = (pos,(wx,wy + read n))
move2 ('W':n) (pos,(wx,wy)) = (pos,(wx - read n,wy))
move2 ('L':n) (pos,(wx,wy)) = case read n of
                                 90 -> (pos, turnL (wx,wy))
                                 180 -> (pos, (-wx,-wy))
                                 270 -> (pos, turnL (-wx,-wy))
                                 360 -> (pos, (wx,wy))
move2 ('R':n) (pos, (wx,wy)) = case read n of
                                 90 -> (pos, turnR (wx,wy))
                                 180 -> (pos, (-wx,-wy))
                                 270 -> (pos, turnR (-wx,-wy))
                                 360 -> (pos, (wx,wy))
move2 ('F':n) ((x,y), (wx,wy)) = ((x + read n * wx, y + read n * wy), (wx,wy))

sol2 [] ((x,y),_) = (x, y, abs x + abs y)
sol2 (m:ms) poswp = sol2 ms (move2 m poswp)

main = do i <- input1
          --mapM_ print i
          print (sol1 i ((0,0), (1,0)))
          print (sol2 i ((0,0), (10,-1)))