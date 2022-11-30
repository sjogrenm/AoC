import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map

import Util


input1 :: IO [Int]
input1 = parseInput read "9.txt"

findPair _ []                           = False
findPair y (x:xs) | (y-x `elem` xs)     = True
findPair y (x:xs)                       = findPair y xs

sol1 pre (x:xs) | findPair x pre    = sol1 pre' xs
                | otherwise         = x
  where pre' = drop 1 pre ++ [x]

sol2 s1 curr (x:xs) | sum curr < s1     = sol2 s1 (curr++[x]) xs
                    | sum curr > s1     = sol2 s1 (tail curr) (x:xs)
                    | otherwise         = (curr, minimum curr, maximum curr)

main = do i <- input1
          --print i
          let (pre,xs) = splitAt 25 i
          let s1 = sol1 pre xs
          print s1
          let (result,mi,ma) = sol2 s1 [] i
          print (mi+ma)
