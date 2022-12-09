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

input = parseInput f "9.txt"

f :: String -> (Char,Int)
f (c:' ':rest) = (c,read rest)


moveHead d pos = case d of
                   'D' -> incY pos
                   'U' -> decY pos
                   'R' -> incX pos
                   'L' -> decX pos


moveTail (hx,hy) (tx,ty)
  | abs (hx - tx) <= 1 && abs (hy - ty) <= 1    = (tx,ty)
  | otherwise                                   = (tx + signum (hx - tx), ty + signum (hy - ty))
--   | hx == tx                = if abs (hy - ty) >= 2 then (tx, ty + signum (hy - ty)) else (tx,ty)
--   | hy == ty                = if abs (hx - tx) >= 2 then (tx + signum (hx - tx), ty) else (tx,ty)

incX (x,y) = (x+1,y)
decX (x,y) = (x-1,y)
incY (x,y) = (x,y+1)
decY (x,y) = (x,y-1)


sol1 i = Set.size $ Set.fromList $ moves (0,0) (0,0) i
  where moves hpos tpos [] = []
        moves hpos tpos ((_,0):is) = moves hpos tpos is
        moves hpos tpos ((d,c):is) = let hpos' = moveHead d hpos
                                         tpos' = moveTail hpos' tpos
                                      in tpos' : moves hpos' tpos' ((d,c-1):is)


sol2 i = Set.size $ Set.fromList $ moves (replicate 10 (0,0)) i
  where moves ks [] = []
        moves ks ((_,0):is) = moves ks is
        moves (h:ts) ((d,c):is) = let h' = moveHead d h
                                      ts' = m h' ts
                                   in last ts' : moves (h':ts') ((d,c-1):is)
        m h [] = []
        m h (t:ts) = let t' = moveTail h t
                      in t' : m t' ts
        


main = do i <- input
        --   print (head i)
          print (sol1 i)
          print (sol2 i)
