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

--input1 :: IO [String]
input1 = do as <- parseInput p "9.txt"
            return (mkArray as)
p s = mkArray s

(x,y) `inMap` m = y `inBounds` m && x `inBounds` (m!y)

height m p@(x,y) | p `inMap` m      = read [m ! y ! x] :: Int
                 | otherwise        = 10

isLocalMin m p = all (> height m p) $ map (height m) $ neighbours p

neighbours (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

localMinima m = [ (x,y) | y <- indices m, x <- indices (m ! y), isLocalMin m (x,y) ]

sol1 m = sum [ 1 + height m p | p <- localMinima m ]

sol2 m = b1 * b2 * b3
  where (b1:b2:b3:_) = sortBy (comparing Down) [ Set.size $ collect Set.empty p | p <- localMinima m ]
        collect visited p = if height m p >= 9 || p `Set.member` visited
                              then visited
                              else foldl collect (p `Set.insert` visited) (neighbours p)

main = do m <- input1
          print (sol1 m)
          print (sol2 m)
