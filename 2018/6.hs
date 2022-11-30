import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
import Data.Array
import Debug.Trace (trace)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as Map

import Util

example = [(1, 1), ( 1, 6), ( 8, 3), ( 3, 4), ( 5, 5), ( 8, 9)]

getInput :: IO [(Int,Int)]
getInput = parseInput p "6.txt"
  where p xs = let [a,b] = words xs
                in (read (init a), read b)

manhattan (a,b) (c,d) = abs (a - c) + abs (b - d)

insidePoints pts = [ (x,y) | (x,y) <- pts, x > minX && x < maxX && y > minY && y < maxY ]
  where xs = map fst pts
        ys = map snd pts
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys

f p pts | manhattan p a == manhattan p b        = Nothing
        | otherwise                             = Just a
  where (a:b:_) = sortOn (manhattan p) pts


sol1 pts =  [ (x,1+length xs) | (Just x:xs) <- sortOn (Down . length) $ group (sort bar), x `elem` is ]
  where xs = map fst pts
        ys = map snd pts
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
        is = insidePoints pts
        bar = [ (f (x,y) pts) | x <- [minX..maxX], y <- [minY..maxY] ]


g p pts = sum (map (manhattan p) pts)

sol2 pts = length $ filter (<10000) bar
  where xs = map fst pts
        ys = map snd pts
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
        is = insidePoints pts
        bar = [ (g (x,y) pts) | x <- [minX..maxX], y <- [minY..maxY] ]

main = do xs <- getInput
          let s1 = sol1 xs
          print s1
          let s2 = sol2 xs
          print s2
