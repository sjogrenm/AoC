import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map

import Util

data Rule = Nop Int | Acc Int | Jmp Int
    deriving Show

data Result = Inf Int | Done Int
    deriving Show

readInstr ('n':'o':'p':' ':num) = Nop (readNum num)
readInstr ('a':'c':'c':' ':num) = Acc (readNum num)
readInstr ('j':'m':'p':' ':num) = Jmp (readNum num)

readNum ('+':xs) = read xs
readNum xs = read xs

input1 = parseInput readInstr "8.txt"

sol1 is visited curr acc
  | curr > snd (bounds is)      = Done acc
  | curr `Set.member` visited   = Inf acc
  | otherwise   = case is ! curr of
                    Nop _ -> sol1 is visited' (curr+1) acc
                    Acc n -> sol1 is visited' (curr+1) (acc+n)
                    Jmp n -> sol1 is visited' (curr+n) acc
  where visited' = Set.insert curr visited

traceI is visited curr acc tr
  | curr > snd (bounds is)      = (Done acc, reverse tr)
  | curr `Set.member` visited   = (Inf acc, reverse tr')
  | otherwise   = case is ! curr of
                    Nop _ -> traceI is visited' (curr+1) acc tr'
                    Acc n -> traceI is visited' (curr+1) (acc+n) tr'
                    Jmp n -> traceI is visited' (curr+n) acc tr'
  where visited' = Set.insert curr visited
        tr' = (curr, is ! curr) : tr

sol1multi is visited curr acc
  | curr > snd (bounds is)      = [Done acc]
  | curr `Set.member` visited   = []
  | otherwise   = case is ! curr of
                    Nop n -> sol1multi is visited' (curr+1) acc ++ fromDone (sol1 is visited' (curr+n) acc)
                    Jmp n -> fromDone (sol1 is visited' (curr+1) acc) ++ sol1multi is visited' (curr+n) acc
                    Acc n -> sol1multi is visited' (curr+1) (acc+n)
  where visited' = Set.insert curr visited
        fromDone (Inf _) = []
        fromDone (Done x) = [Done x]

main = do i <- input1
          --print i
          let is = mkArray i
          --print (sol1 is Set.empty 0 0)
          --let (s1, tr) = traceI is Set.empty 0 0 []
          --mapM_ print tr
          --print s1
          let sols = sol1multi is Set.empty 0 0
          print (sols)
          --print (sol2 i)
