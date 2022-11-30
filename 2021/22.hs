{-# LANGUAGE FlexibleContexts #-}

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

input = parseInput p "22.txt"

ex = map p [ "on x=-20..26,y=-36..17,z=-47..7"
           , "on x=-20..33,y=-21..23,z=-26..28"
           , "on x=-22..28,y=-29..23,z=-38..16"
           , "on x=-46..7,y=-6..46,z=-50..-1"
           , "on x=-49..1,y=-3..46,z=-24..28"
           , "on x=2..47,y=-22..22,z=-23..27"
           , "on x=-27..23,y=-28..26,z=-21..29"
           , "on x=-39..5,y=-6..47,z=-3..44"
           , "on x=-30..21,y=-8..43,z=-13..34"
           , "on x=-22..26,y=-27..20,z=-29..19"
           , "off x=-48..-32,y=26..41,z=-47..-37"
           , "on x=-12..35,y=6..50,z=-50..-2"
           , "off x=-48..-32,y=-32..-16,z=-15..-5"
           , "on x=-18..26,y=-33..15,z=-7..46"
           , "off x=-40..-22,y=-38..-28,z=23..41"
           , "on x=-16..35,y=-41..10,z=-47..6"
           , "off x=-32..-23,y=11..30,z=-14..3"
           , "on x=-49..-5,y=-3..45,z=-29..18"
           , "off x=18..30,y=-20..-8,z=-3..13"
           , "on x=-41..9,y=-7..43,z=-33..15"
           , "on x=-54112..-39298,y=-85059..-49293,z=-27449..7877"
           , "on x=967..23432,y=45373..81175,z=27513..53682"]

p xs = case words xs of
         ["on",cs] -> (True,parseCoords cs)
         ["off",cs] -> (False,parseCoords cs)

parseCoords cs = case splitOn ',' cs of
                   ['x':'=':x, 'y':'=':y, 'z':'=':z] -> (foo x, foo y, foo z)
  where foo xs = case splitOn '.' xs of
                   [a,"",b] -> (read a :: Int, read b :: Int)

(a,b) `overlaps` (c,d) = a `isInRange` (c,d) || b `isInRange` (c,d)

removeIrrelevant xs = filter ok xs
  where ok (_,(x,y,z)) = x `overlaps` (-50,50) || y `overlaps` (-50,50) || z `overlaps` (-50,50)

isOn (x,y,z) cs = isOn' cs False
  where isOn' [] acc = acc
        isOn' ((b,(xs,ys,zs)):cs) acc = isOn' cs (if x `isInRange` xs && y `isInRange` ys && z `isInRange` zs then b else acc)

countOn cs = length [ () | x <- [-50..50], y <- [-50..50], z <- [-50..50], isOn (x,y,z) cs ]

sol1 cs = countOn (removeIrrelevant cs)


type Range = (Int,Int)
type Cuboid = (Range,Range,Range)

xr = fst3
yr = snd3
zr = thd3

lo = fst
hi = snd


--  a------b
--      c-----d
split :: Cuboid -> Cuboid -> Maybe ([Cuboid],Cuboid,[Cuboid])
split c1 c2
  | xr c1 `overlaps` xr c2 && yr c1 `overlaps` yr c2 && zr c1 `overlaps` zr c2
                    = Just ([],undefined,[])
  | otherwise       = Nothing

--  a=====b
--     c=====d
-- ->
--  a==
--     c==b
--         ==d

--  a=======b
--  c===d
segments (a,b) (c,d) | a < c     = (a,c-1) : segments (c,b) (c,d)
                     | d < b     = (d+1,b) : segments (a,d) (c,d)
                     | otherwise = [(a,b)]


splitRange (xl, xh) (xl', xh') | xl < xl'  = (xl, xl'-1) : splitRange (xl', xh) (xl', xh')
                               | xh > xh'  = (xh'+1, xh) : splitRange (xl, xh') (xl', xh')
                               | otherwise = [(xl, xh)]

add :: [Cuboid] -> Cuboid -> [Cuboid]
add [] b = [b]
add (a:as) b = case split a b of
                 Just (cs,d,es) -> cs ++ [d] ++ foldl add as es
                 Nothing      -> a : add as b

sub :: [Cuboid] -> Cuboid -> [Cuboid]
sub = undefined


main = do i <- input
          print (sol1 i)