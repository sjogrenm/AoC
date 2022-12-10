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

input = parseInput id "3.txt"
  where r :: String -> (String,String)
        r xs = splitAt (length xs `div` 2) xs


val c | isUpper c   = ord c - ord 'A' + 27
      | otherwise   = ord c - ord 'a' + 1

sol1 xs = sum
          [ let (a,b) = splitAt (length x `div` 2) x
                c:_ = intersect a b
             in val c
          | x <- xs ]

chunk [] = []
chunk (a:b:c:xs) = [a,b,c] : chunk xs

sol2 xs = sum
          [ let d:_ = a `intersect` b `intersect` c
             in val d
          | [a,b,c] <- chunk xs ]


main = do i <- input
        --   print (head i)
          print (sol1 i)
          print (sol2 i)
