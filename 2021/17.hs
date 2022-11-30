import Data.Maybe
import Data.Ord
import Data.List
import Data.Char
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Data.Bits

import Util

xLimit = (257,286)
yLimit = (-101,-57)

--xLimit = (20,30)
--yLimit = (-10,-5)

inTarget (x,y) = x >= fst xLimit && x <= snd xLimit && y >= fst yLimit && y <= snd yLimit

generate xs ys = [ (xv, yv, result)
                 | xv <- xs, yv <- ys
                 , let result = trajectory (xv,yv)
                 , any inTarget result ]

sol1 = maximumBy (comparing thd3) [ (x,y, maximum $ map snd result) | (x,y,result) <- generate [1..40] [50..120] ]

sol2 = [ (xv,yv) | (xv,yv,_) <- (generate [1..300] [-150..200]) ]

step ((xv,yv), (xp,yp)) = ((xv', yv-1), (xp+xv, yp+yv))
  where xv' = if xv < 0 then min 0 (xv+1) else max 0 (xv-1)

trajectory v = takeWhile p $ map snd $ iterate step (v,(0,0))
  where p (xp,yp) = xp <= snd xLimit && yp >= fst yLimit
