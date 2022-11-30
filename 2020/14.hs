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
import qualified Data.Map.Strict as Map

import Util

data Instr = Mask String | Assign Int Int
    deriving Show

input1 = parseInput parseInputLine "14.txt"
parseInputLine xs = case splitOn '=' xs of
                 ["mask ", ' ':mask] -> Mask mask
                 [addr, ' ':val] -> Assign (parseAddr addr) (read val)
parseAddr ('m':'e':'m':'[':xs) = read (takeWhile isDigit xs)

applyMask xs val = f xs (length xs - 1) val
  where f [] _ val = val
        f (x:xs) exp val = case x of
                             'X' -> f xs (exp - 1) val
                             '1' -> f xs (exp - 1) (setBit val exp)
                             '0' -> f xs (exp - 1) (clearBit val exp)

iter1 [] mask mem = mem
iter1 (Mask s:is) mask mem = iter1 is s mem
iter1 (Assign a v:is) mask mem = iter1 is mask mem'
  where val = applyMask mask v
        mem' = Map.insert a val mem

sol1 is = foldr1 (+) mem
  where mem = iter1 is (replicate 36 'X') Map.empty

applyAddrMask xs val = f xs (length xs - 1) [val]
  where f [] _ vs = vs
        f (x:xs) exp vs = case x of
                            '0' -> f xs (exp - 1) vs
                            '1' -> f xs (exp - 1) [ setBit v exp | v <- vs ]
                            'X' -> f xs (exp - 1) $ concat [ [ clearBit v exp, setBit v exp ] | v <- vs ]

iter2 :: [Instr] -> String -> Map.Map Int Int -> Map.Map Int Int
iter2 [] mask mem = mem
iter2 (Mask s:is) mask mem = iter2 is s mem
iter2 (Assign a v:is) mask mem = iter2 is mask mem'
  where addrs = applyAddrMask mask a
        mem' = foldr (\a -> Map.insert a v) mem addrs

sol2 is = foldr1 (+) mem
  where mem = iter2 is (replicate 36 'X') Map.empty


ex = map parseInputLine ["mask = 000000000000000000000000000000X1001X","mem[42] = 100","mask = 00000000000000000000000000000000X0XX","mem[26] = 1"]

main = do i <- input1
          --print i
          print (sol1 i)
          print (sol2 i)