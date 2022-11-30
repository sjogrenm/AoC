import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Tree

import Util
import IntCode

input1 = parseInput pf "6.txt"

pf xs = let [a,b] = splitOn ')' xs in (a,b)

test = map pf $ words "COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L K)YOU I)SAN"

mkTree xs root = unfoldTree f root
  where f n = (n, lookupAll n xs)

paths :: String -> Tree String -> [[String]]
paths dest t | rootLabel t == dest      = [[]]
             | otherwise                = map (rootLabel t :) $ concatMap (paths dest) (subForest t)

sol1 t = sum [ d * length l | (d,l) <- zip [0..] (levels t) ]

sol2 t = length (drop common youPath) + length (drop common sanPath)
  where [youPath] = paths "YOU" t
        [sanPath] = paths "SAN" t
        eq (a,b) = a == b
        common = length $ takeWhile eq (zip youPath sanPath)

main = do i <- input1
          let t = mkTree i "COM"
          --let k = [("COM","B"), ("B", "C"), ("C", "D"), ("B", "G")]
          --let t = mkTree k "COM"
          --putStrLn $ drawTree t
          --print $ levels t
          print $ sol1 t
          --print $ sol2 t
          --let t = mkTree test "COM"
          print $ sol2 t
