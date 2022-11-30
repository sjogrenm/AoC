import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Lazy as Map
import Data.Tree
import Data.Ratio

import Util
--import IntCode
--import qualified IntCodeM

input1 = fmap buildMap $ readFile "10.txt"

buildMap :: String -> Array (Int,Int) Bool
buildMap xs = array ((0,0),(xmax,ymax)) [ ((x,y),a == '#') | (l,y) <- zip ls [0..], (a,x) <- zip l [0..] ]
  where ls = lines xs
        ymax = length ls - 1
        xmax = length (head ls) - 1


sol1 a = maximumBy (comparing fst) [ (count a (x,y), (x,y)) | ((x,y),True) <- assocs a ]
  where (xmax,ymax) = bounds a

count a (x,y) = length [ () | ((x2,y2),True) <- assocs a, not $ any (a !) $ path (x,y) (x2,y2) ] - 1

path (a,b) (c,d) = drop 1 $ takeWhile (/= (c,d)) $ iterate (\(x,y) -> (x+dx,y+dy)) (a,b)
  where dx1 = c - a
        dy1 = d - b
        dx = dx1 `div` gcd dx1 dy1
        dy = dy1 `div` gcd dx1 dy1

type Direction a = (Bool, Maybe (Ratio a))

angleDist :: (Int,Int) -> (Int,Int) -> (Direction Int, Int)
angleDist (a,b) (c,d)
  | a == c      = ((b < d, Nothing), abs (b - d))
  | otherwise   = ((a > c, Just $ (b - d) % (a - c)), abs (a - c))


example1 = buildMap ".#..#\n.....\n#####\n....#\n...##"
example2 = buildMap "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"


mapping a = maximumBy (comparing Map.size)
            [ fmap sort $ Map.fromListWith (++) $ map annotate (before ++ after)
            | (before, point:after) <- zip (inits points) (tails points)
            , let annotate p = let (dir,dist) = angleDist point p in (dir, [(dist,p)])
            ]
  where points = [ p | (p,True) <- assocs a ]

sol2 a = map snd . concat . transpose . Map.elems . mapping $ a

example4 = buildMap ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##"

main = do i <- input1
          --print $ sol1 example1
          --print $ sol1 example2
          print $ sol1 i
          print $ sol2 i !! 199
          --print $ runIO example1 []
          --print $ runIO example2 []
          --print $ runIO example3 []
          --print $ sol1 $ fromList input1
          --print $ sol2 $ fromList input1
          --print $ fst $ IntCodeM.runIntCode (V.fromList input1) [1]
          --print $ fst $ IntCodeM.runIntCode (V.fromList input1) [2]
