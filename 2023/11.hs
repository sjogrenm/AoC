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

input :: IO [[Char]]
input = parseInput id "11.txt"

emptyLines :: [[Char]] -> [Int]
emptyLines is = f is 0
  where f []     _ = []
        f (i:is) k = (if all (=='.') i then [k] else []) ++ f is (k+1)

emptyColumns :: [[Char]] -> [Int]
emptyColumns is = emptyLines (transpose is)

galaxies :: [[Char]] -> [(Int,Int)]
galaxies is = [ (x,y) | (i,y) <- zip is [0..], ('#',x) <- zip i [0..] ]

sol f is = sum [ dist g1 g2 | (g1,g2) <- pairs gs ]
  where eL = emptyLines is
        eC = emptyColumns is
        gs = galaxies is
        dist (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2) + f * (expand y1 y2 eL + expand x1 x2 eC)

expand a b empties = length [ n | n <- empties, n > mi && n < ma ]
  where mi = min a b
        ma = max a b

sol1 is = sol 1 is
sol2 is = sol 999999 is

main = do
        i <- input
        -- print (galaxies i)
        -- print (emptyLines i)
        -- print (emptyColumns i)
        -- print i
        print (sol1 i)
        print (sol2 i)
        return ()

