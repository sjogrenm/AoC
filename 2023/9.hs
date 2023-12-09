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

input :: IO [[Int]]
input = parseInput f "9.txt"
  where f = map read . words

iter acc (a:b:cs) = iter (b-a : acc) (b:cs)
iter acc [y] = (y, reverse acc)

sol1 is = sum $ map (sol1' []) is

sol1' :: [Int] -> [Int] -> Int
sol1' acc is | all (==0) is     = sum acc
             | otherwise        = let (x, ys) = iter [] is
                                   in sol1' (x:acc) ys


sol2 is = sum $ map (sol2' []) is

sol2' acc is | all (==0) is     = foldr1 (-) (reverse acc)
             | otherwise        = let x = head is
                                      (_, ys) = iter [] is
                                   in sol2' (x:acc) ys

main = do
        i <- input
        -- print (iter [] $ head i)
        -- print i
        print (sol1 i)
        print (sol2 i)
        return ()

