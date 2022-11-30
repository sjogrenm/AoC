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
import Data.Graph

import Util

example = [('C', 'A'), ( 'C', 'F'), ( 'A', 'B'), ( 'A', 'D'), ( 'B', 'E'), ( 'D', 'E'), ('F', 'E')]

getInput :: IO [(Char,Char)]
getInput = parseInput p "7.txt"
  where p xs = let [[a],[b]] = filter (\x -> length x == 1) $ words xs
                in (a, b)

dropDup (a:b:cs) | a == b       = dropDup (b:cs)
                 | otherwise    = a : dropDup (b:cs)
dropDup cs = cs


myG xs = [ (ins n, n, outs n) | n <- nodes ]
  where nodes = dropDup $ sort (map fst xs `union` map snd xs)
        ins c = [ f | (f,t) <- xs, t == c ]
        outs c = [ t | (f,t) <- xs, f == c ]

gubbins [] = []
gubbins g = x : gubbins g'
  where (x:_) = sort [ n | ([],n,os) <- g ]
        g' = [ (is\\[x], n, os) | (is,n,os) <- g, n /= x ]

sol1 xs = gubbins (myG xs)


sol2 ws xs = gubbins2 ws [] (myG xs)

gubbins2 _ _ [] = []
gubbins2 ws ps g = case foo of
                     [] -> wait ws ps g
                     _  -> gubbins2 ws' ([ (w, c, time c) | (w,c) <- foo ] ++ ps) g
  where xs = [ n | ([],n,os) <- g, n `notElem` psn ]
        psn = [ n' | (_,n',_) <- ps ]
        foo = zip ws xs
        ws' = drop (length foo) ws

wait ws ps g = done ++ gubbins2 (ws++ws') [ (w',c',t'-t) | (w',c',t') <- ps', t' > t ] g'
  where ps' = sortBy (comparing thd3) ps
        t = thd3 (head ps')
        (ws',done) = unzip [ (w',(c',t')) | (w',c',t') <- ps', t' == t ]
        (xs',_) = unzip done
        g' = [ (is\\xs', n, os) | (is,n,os) <- g, n `notElem` xs' ]



time c = ord c - ord 'A' + 61

main = do xs <- getInput
          let s1 = sol1 xs
          print s1
          let s2 = sol2 [1..5] xs
          print s2
          print $ sum . snd . unzip $ s2
