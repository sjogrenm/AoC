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

getInput = parseInput id "5.txt"

sol1 xs = sol1' xs []

sol1' (a:b:xs) ys | a /= b && toLower a == toLower b    = case ys of
                                                            z:zs -> sol1' (z:xs) zs
                                                            _    -> sol1' xs ys
sol1' (x:xs) ys = sol1' xs (x:ys)
sol1' [] ys = ys

sol2 xs = sortOn (length . fst) yss
  where yss = zip (map sol1 [ dropAll c (dropAll (toLower c) xs) | c <- ['A'..'Z'] ]) ['A'..'Z']
        dropAll c [] = []
        dropAll c (a:as) | c == a       = dropAll c as
                         | otherwise    = a : dropAll c as

main = do [i] <- getInput
          let s1 = sol1 i
          print $ length s1
          let xs = sol2 i
          print [ (length y, c) | (y,c) <- xs ]
