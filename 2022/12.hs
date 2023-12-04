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

data Op = Plus Int | Times Int | Square
    deriving (Eq,Show)

data Monkey = Monkey [Int] Op Int Int Int Int
    deriving (Eq,Show)

input = [
    Monkey [93,98] (Times 17) 19 5 3 0,
    Monkey [95,72,98,82,86] (Plus 5) 13 7 6 0,
    Monkey [85,62,82,86,70,65,83,76] (Plus 8) 5 3 0 0,
    Monkey [86,70,71,56] (Plus 1) 7 4 5 0,
    Monkey [77,71,86,52,81,67] (Plus 4) 17 1 6 0,
    Monkey [89,87,60,78,54,77,98] (Times 7) 2 1 4 0,
    Monkey [69,65,63] (Plus 6) 3 7 2 0,
    Monkey [89] Square 11 0 2 0
  ]

ex = [
    Monkey [79,98] (Times 19) 23 2 3 0,
    Monkey [54,65,75,74] (Plus 6) 19 2 0 0,
    Monkey [79,60,97] Square 13 1 3 0,
    Monkey [74] (Plus 3) 17 0 1 0
  ]


worry (Plus k)  x = (x + k) `div` 3
worry (Times k) x = (x * k) `div` 3
worry Square    x = (x * x) `div` 3


turn (Monkey [] _ _ _ _ _) = []
turn (Monkey (x:xs) op d mt mf c) = let x' = worry op x
                                        m = if x' `mod` d == 0 then mt else mf
                                     in (m,x') : turn (Monkey xs op d mt mf c)

mround ms = round' ms [0..] [] []

round' [] _ pending ms = [ Monkey (findAll i pending) op d mt mf (c+length xs) | (Monkey xs op d mt mf c, i) <- zip (reverse ms) [0..] ]

round' (m:ms) (i:is) pending vms = round' ms' is pending' (m:vms)
  where up = turn m
        pending' = pending ++ [ (k,y) | (k,y) <- up, k < i ]
        ms' = [ Monkey (xs ++ findAll j up) op d mt mf c | (Monkey xs op d mt mf c,j) <- zip ms [i+1..] ]

findAll a xs = [ y | (b,y) <- xs, a == b ]


sol1 ms = let [a,b] = take 2 $ sortBy (comparing negate) [ c | Monkey _ _ _ _ _ c <- iterate mround ms !! 20 ]
           in a * b


worry2 f (Plus k)  x = (x + k) `mod` f
worry2 f (Times k) x = (x * k) `mod` f
worry2 f Square    x = (x * x) `mod` f

turn2 f (Monkey [] _ _ _ _ _) = []
turn2 f (Monkey (x:xs) op d mt mf c) = let x' = worry2 f op x
                                           m = if x' `mod` d == 0 then mt else mf
                                        in (m,x') : turn2 f (Monkey xs op d mt mf c)

mround2 ms = round2' f ms [0..] [] []
  where ds = [ d | Monkey _ _ d _ _ _ <- ms ]
        f = product ds `div` foldr1 gcd ds

round2' _ [] _ pending ms = [ Monkey (findAll i pending) op d mt mf (c+length xs) | (Monkey xs op d mt mf c, i) <- zip (reverse ms) [0..] ]

round2' f (m:ms) (i:is) pending vms = round2' f ms' is pending' (m:vms)
  where up = turn2 f m
        pending' = pending ++ [ (k,y) | (k,y) <- up, k < i ]
        ms' = [ Monkey (xs ++ findAll j up) op d mt mf c | (Monkey xs op d mt mf c,j) <- zip ms [i+1..] ]

sol2 ms = let [a,b] = take 2 $ sortBy (comparing negate) [ c | Monkey _ _ _ _ _ c <- iterate mround2 ms !! 10000 ]
           in a * b

main = do print (sol1 input)
          print (sol2 input)
