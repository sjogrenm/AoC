import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import Util

data Cmd = Forward Int | Dwn Int | Up Int
    deriving (Eq, Show)

ex = ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]

input1 = parseInput id "3.txt"
--   where r :: String -> [Int]
--         r xs = map (\x -> read [x]) xs

mostCommonBit xs = f xs 0 0
  where f [] z o | z > o    = Just 0
                 | o > z    = Just 1
                 | otherwise    = Nothing
        f ('0':xs) z o = f xs (z+1) o
        f ('1':xs) z o = f xs z (o+1)

toNum bs = sum $ zipWith (\b p -> b * 2^p) (reverse bs) [0..]

sol1 cs = toNum gamma * toNum eps
  where xs = transpose cs
        gamma = map (fromJust . mostCommonBit) xs
        eps = map (\b -> 1 - b) gamma

sol2 xs = oxy 0 xs * co2 0 xs

oxy _ [x] = toNum [ read [d] | d <- x ]
oxy k xs = oxy (k+1) [ x | x <- xs, read [x!!k] == mc ]
  where mc = case mostCommonBit (map (!!k) xs) of
                  Just b  -> b
                  Nothing -> 1

co2 _ [x] = toNum [ read [d] | d <- x ]
co2 k xs = co2 (k+1) [ x | x <- xs, read [x!!k] == lc ]
  where lc = case mostCommonBit (map (!!k) xs) of
               Just b  -> 1 - b
               Nothing -> 0

main = do i <- input1
          print (sol1 i)
          print (sol2 i)
