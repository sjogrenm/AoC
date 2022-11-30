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

exP = mkArray "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
exDots = mkDots $ lines "#..#.\n#....\n##..#\n..#..\n..###"

mkDots s = Set.fromList [ (x,y) | (y,cs) <- zip [0..] s, (x,c) <- zip [0..] cs, c == '#' ]

fromBits bs = fb bs 0
  where fb [] acc = acc
        fb (b:bs) acc = fb bs (2 * acc + if b then 1 else 0)

input = do s <- readFile "20.txt"
           let p:"":rest = lines s
           return (mkArray p, mkDots rest)


--enhance :: Array Int Char -> Set.Set (Int,Int) -> [Bool]
enhance p oob dots = Set.fromList [ (x,y) | x <- [x1-1..x2+1], y <- [y1-1..y2+1], let ix = fromBits (bits (x,y)), p ! ix == '#' ]
  where ((x1,y1),(x2,y2)) = dotBounds dots
        inSet (x,y) | x < x1 || x > x2 || y < y1 || y > y2    = oob
                    | otherwise                               = (x,y) `Set.member` dots
        bits (x,y) = [ inSet (x',y') | y' <- [y-1..y+1], x' <- [x-1..x+1] ]

dotBounds dots = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where (xs,ys) = unzip (Set.toList dots)

enhance2 p = enhance p True . enhance p False

sol1 p dots = Set.size $ enhance2 p dots

sol2 p dots = Set.size $ iterate (enhance2 p) dots !! 25

main = do (p,dots) <- input
          --print (dotBounds dots)
          --print $ dotBounds False dots
          --print $ dotBounds $ enhance p dots
          print (sol1 p dots)
          print (sol2 p dots)