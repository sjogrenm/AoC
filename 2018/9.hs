import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
import Data.Array
import Debug.Trace (trace)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as Map

import Util

example = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]

getInput :: IO [Int]
getInput = (map read . head) `fmap` parseInput words "9.txt"

players = 404
marbles = 71852


initial = ([], 0, [])

initialScores = listArray (1,players) (repeat 0)

-- [0],4,[2,1,3]  ->  [2,4,0],5,[1,3]
addMarble (ls,c,r:rs) m = (r:c:ls, m, rs)
-- [1,2,0],3,[]  ->  [0,3],4,[2,1]
addMarble (ls,c,[]) m
  = case reverse ls of
      a:as -> ([a,c], m, as)
      []   -> ([], m, [c])

-- [5,21,10,20,2,19,9,18,4,17,8,16,0], 22, [11,1,12,6,13]
--   ->
-- [18,4,17,8,16,0], 19, [2,20,10,21,5,22,11,1,12,6,13]
removeMarble (ls,c,rs) = case splitAt 5 ls of
                           (as, c':b:bs) -> ((bs, c', reverse as ++ c:rs), b)
                           (_, []) -> case splitAt 5 (ls ++ reverse (c:rs)) of
                                        (as, c:b:bs) -> ((bs, c, reverse as), b)


pr (xs, c, ys) = show (reverse xs) ++ " (" ++ show c ++ ") " ++ show ys

getPlayer m = case m `mod` players of
                0 -> players
                p -> p


add (foo,scores) m
  | m `mod` 23 == 0     = (foo', scores // [(p, x + m + scores ! p)])
  where p = getPlayer m
        (foo', x) = removeMarble foo

add (foo,scores) m = (addMarble foo m, scores)


sol ms = let (result,scores) = foldl add (initial, initialScores) [1..ms]
          in maximum (elems scores)

sol1 = sol marbles

sol2 = sol (100*marbles)
