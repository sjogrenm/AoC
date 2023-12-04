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

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator

import Util

ex = [ (2,2,2)
     , (1,2,2)
     , (3,2,2)
     , (2,1,2)
     , (2,3,2)
     , (2,2,1)
     , (2,2,3)
     , (2,2,4)
     , (2,2,6)
     , (1,2,5)
     , (3,2,5)
     , (2,1,5)
     , (2,3,5)
     ]

input :: IO [(Int,Int,Int)]
input = parseInput f "18.txt"
  where f xs = let [a,b,c] = splitOn ',' xs
                in (read a, read b, read c)

sol1 xs = sum [ 6 - length [ () | y <- neighbours a, Set.member y s ] | a <- xs ]
  where s = Set.fromList xs
        neighbours (x,y,z) = [(x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z),(x,y,z+1),(x,y,z-1)]

main = do i <- input
          print (sol1 i)