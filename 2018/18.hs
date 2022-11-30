import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
import Data.Array
import Debug.Trace (trace, traceShow)
import qualified Data.Vector as V
--import Data.Vector ((!))
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies
import Data.Graph as G
import Data.Bits

import Util


--getInput :: IO (Int, Int, Set.Set (Int,Int))
getInput = parseInput id "18.txt"

example =
  [ ".#.#...|#."
  , ".....#|##|"
  , ".|..|...#."
  , "..|#.....#"
  , "#.#|||#|#|"
  , "...#.||..."
  , ".|....|..."
  , "||...#|.#|"
  , "|.||||..|."
  , "...#.|..|." ]


sol1' [] = []
sol1' [a,b] = [foo $ zip3 a b (repeat '.')]
sol1' (a:b:c:ds) = (foo $ zip3 a b c) : sol1' (b:c:ds)

sol1iter (a:b:cs) = (foo $ zip3 (repeat '.') a b) : sol1' (a:b:cs)

iterations i = iterate sol1iter i

sol1 xs = score yss
  where yss = iterations xs !! 10

score yss = count '|' ys * count '#' ys
  where ys = concat yss

foo' [] = []
foo' [a,b] = [bar a b ('.','.','.')]
foo' (a:b:c:ds) = bar a b c : foo' (b:c:ds)

foo (a:b:cs) = bar ('.','.','.') a b : foo' (a:b:cs)

bar (a,b,c) (d,e,f) (g,h,i)
  = case e of
        '.' | count '|' neighbours >= 3         -> '|'
        '|' | count '#' neighbours >= 3         -> '#'
        '#' | count '|' neighbours < 1 || count '#' neighbours < 1      -> '.'
        _ -> e

  where neighbours = [a,b,c,d,f,g,h,i]

count x xs = length (filter (==x) xs)


sol2 i = [ (100*n, score (is !! (100*n))) | n <- [1..] ]
  where is = iterations i

main = do
        i <- getInput
        --mapM_ print i
        --print "---"
        --print (sol1 i)
        mapM_ print (sol2 i)

cycle_ = [ 218750 -- 7200
         , 215404 -- 7300
         , 215760
         , 221676
         , 210528
         , 223728
         , 208603 -- 7800
         ] -- 218750   7900

end :: Int
end = 1000000000

afterN n = cycle_ !! (((n - 7200) `div` 100) `mod` 7)
