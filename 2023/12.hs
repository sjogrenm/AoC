import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Util

input :: IO [(String, [Int])]
input = parseInput f "12.txt"
  where f s = let [a,b] = words s
                  cs = splitOn ',' b
               in (a, map read cs)

generate :: String -> [String]
generate [] = [[]]
generate ('?':xs) = map ('.':) ys ++ map ('#':) ys
  where ys = generate xs
generate (x:xs) = map (x:) (generate xs)

valid xs ns = valid' xs' ns
  where xs' = filter (/= "") $ splitOn '.' xs

valid' [] [] = True
valid' (x:xs) (n:ns) = length x == n && valid' xs ns
valid' _ _ = False


sol1 is = sum $ map f is
  where f (xs,ns) = length [ s | s <- generate xs, valid s ns ]

sol2 is = sum $ map f is
  where f (xs,ns) = let xs' = xs ++ (concat $ replicate 4 $ "?" ++ xs)
                        ns' = concat $ replicate 5 ns
                     in length [ s | s <- generate xs', valid s ns' ]

main = do
        i <- input
        -- print (galaxies i)
        -- print (emptyLines i)
        -- print (emptyColumns i)
        -- print i
        -- print (sol1 i)
        print (sol2 i)
        return ()

