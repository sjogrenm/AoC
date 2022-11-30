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

import Util

input1 :: IO (Set.Set (Int,Int), [(Char,Int)])
input1 = do is <- readFile "13.txt"
            let [dotText,foldText] = splitOn "" (lines is)
            let dots = [ (read x, read y) | [x,y] <- map (splitOn ',') dotText ]
            let folds = [ (axis, read val) | tmp <- map (splitOn ' ') foldText, let [[axis],val] = splitOn '=' (tmp !! 2) ]
            return (Set.fromList dots, folds)

fold dots ('x', fv) = Set.map (\(x,y) -> (if x > fv then 2*fv - x else x, y)) dots
fold dots ('y', fv) = Set.map (\(x,y) -> (x, if y > fv then 2*fv - y else y)) dots

sol1 dots f = Set.size $ fold dots f

sol2 dots [] = pretty $ sortBy (comparing (\(x,y) -> (y,x))) $ Set.toList dots
sol2 dots (f:fs) = sol2 (fold dots f) fs

pretty cs = pretty' cs (0,0)
pretty' [] _ = []
pretty' ((x,y):cs) (xp,yp) | y > yp     = '\n' : pretty' ((x,y):cs) (0,yp+1)
                           | x > xp     = ' ' : pretty' ((x,y):cs) (xp+1,yp)
                           | otherwise  = '#' : pretty' cs (xp+1,yp)

main = do (dots,folds) <- input1
          --print dots
          --print folds
          print (sol1 dots (head folds))
          putStrLn (sol2 dots folds)
