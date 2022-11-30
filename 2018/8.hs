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
getInput = (map read . head) `fmap` parseInput words "8.txt"


data Tree a = Tree [Tree a] a
        deriving Show

tree :: [Int] -> Tree [Int]
tree xs = t
  where ([t], []) = tree' xs

tree' (cn:mn:xs) = ([Tree children meta], rest)
  where (children, ys) = tree'' cn xs
        (meta, rest) = splitAt mn ys
tree' [] = ([], [])

tree'' :: Int -> [Int] -> ([Tree [Int]], [Int])
tree'' 0 xs = ([], xs)
tree'' n xs = let (t, ys) = tree' xs
                  (ts, zs) = tree'' (n-1) ys
               in (t++ts, zs)



--tree' _ _ _ [] = (Tree [] [], [])

sol1 (Tree children meta) = sum (meta ++ map sol1 children)

sol2 (Tree [] meta) = sum meta
sol2 (Tree children meta) = sum [ maybe 0 sol2 (lookup m stuff)
                                | m <- meta ]
  where stuff = zip [1..] children

main = do xs <- getInput
          let t = tree xs
          let s1 = sol1 t
          print s1
          let s2 = sol2 t
          print s2
