import Data.Bits
import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace, traceShow)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as Map

import Util

constraintText = ["departure location: 27-840 or 860-957"
        ,"departure station: 28-176 or 183-949"
        ,"departure platform: 44-270 or 277-967"
        ,"departure track: 33-197 or 203-957"
        ,"departure date: 47-660 or 677-955"
        ,"departure time: 45-744 or 758-971"
        ,"arrival location: 42-636 or 642-962"
        ,"arrival station: 44-243 or 252-962"
        ,"arrival platform: 46-428 or 449-949"
        ,"arrival track: 25-862 or 876-951"
        ,"class: 26-579 or 585-963"
        ,"duration: 38-683 or 701-949"
        ,"price: 41-453 or 460-970"
        ,"route: 48-279 or 292-963"
        ,"row: 33-617 or 637-955"
        ,"seat: 39-328 or 351-970"
        ,"train: 35-251 or 264-957"
        ,"type: 25-380 or 389-951"
        ,"wagon: 42-461 or 480-965"
        ,"zone: 33-768 or 789-954"]

data Constraint = C String [(Int,Int)]
    deriving Show

instance Eq Constraint where
  C a _ == C b _    = a == b

constraints = map f constraintText
  where f s = let [x,y] = splitOn ':' s
                  ["",ab,"or",cd] = splitOn ' ' y
                  [a,b] = map read $ splitOn '-' ab
                  [c,d] = map read $ splitOn '-' cd
               in C x [(a,b),(c,d)]

n `satisfies` C _ xs = or [ x1 <= n && n <= x2 | (x1,x2) <- xs ]

myTicket = [83,53,73,139,127,131,97,113,61,101,107,67,79,137,89,109,103,59,149,71]

otherTickets :: IO [[Int]]
otherTickets = parseInput f "16b.txt"
  where f s = map read $ splitOn ',' s

isValidTicket cs ot = and [ any (f `satisfies`) cs | f <- ot ]

sol1 ots cs = sum [ sum [ f | f <- ot, not $ any (f `satisfies`) cs ] | ot <- ots ]

ex :: [[Int]]
ex = [[7,3,47]
     ,[40,4,50]
     ,[55,2,20]
     ,[38,6,12]]
exCons = [C "class" [(1,3), (5,7)], C "row" [(6,11),(33,44)], C "seat" [(13,40), (45,50)]]


ex2 :: [[Int]]
ex2 = [[3,9,18]
      ,[15,1,5]
      ,[5,14,9]]
exCons2 = [C "class" [(0,1), (4,19)], C "row" [(0,5), (8,19)], C "seat" [(0,13), (16,19)]]

validConstraintsFor cs fs = [ c | c <- cs, all (`satisfies` c) fs ]

iter2 cs [] = cs
iter2 cs (i:is) = case cs !! i of
                    [c] -> iter2 [ if i == j then c2 else c2 \\ [c] | (c2,j) <- zip cs [0..] ] is
                    _   -> iter2 cs is

sol2 ots cs = product [ myTicket !! i | i <- depFields ]
  where validCs = [ validConstraintsFor cs fs | fs <- transpose ots ]
        ix = [0..length validCs-1]
        f old = let new = iter2 old ix
                 in if new == old then new else f new
        result = f validCs
        depFields = [ i | ([C fname _], i) <- zip result [0..], "departure" `isPrefixOf` fname ]

main = do ot <- otherTickets
          --print ot
          print (sol1 ot constraints)
          let ot2 = filter (isValidTicket constraints) ot
          --print (length ot, length ot2)
          print (sol2 ot2 constraints)
          --print (sol' input 2020)
          --print (sol' input 30000000)
          --print (sol2 input)