import Data.Maybe
import Data.Ord
import Data.List
import Data.Char
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import Util

input1 = do is <- readFile "14.txt"
            let [[template],rulesText] = splitOn "" (lines is)
            let rules = [ (pattern,ins) | [pattern,"->",[ins]] <- map (splitOn ' ') rulesText ]
            return (template,Map.fromList rules)

step m (a:b:cs) = a : (fromJust $ Map.lookup [a,b] m) : step m (b:cs)
step _ rest = rest

sol n m template = last sn' - head sn'
  where sn = iterate (step m) template !! n
        sn' = sort $ map length $ group $ sort sn

toPairs template = Map.fromListWith (+) [ (p,1) | p <- ps template ]
  where ps (a:b:cs) = [a,b] : ps (b:cs)
        ps rest = []

step' :: Map.Map String Char -> Map.Map String Int -> Map.Map String Int
step' rules ps = Map.fromListWith (+) $ concat [ [([a,z],n), ([z,b],n)] | ([a,b],n) <- Map.toList ps
                                                                        , let Just z = Map.lookup [a,b] rules ]

sol' n rules template = (last sn - head sn) `div` 2
  where ps = toPairs template
        f = head template
        l = last template
        ps' = iterate (step' rules) ps !! n
        m = Map.fromListWith (+) $ (f,1) : (l,1) : concat [ [(a,z),(b,z)] | ([a,b],z) <- Map.toList ps' ]
        sn = map snd $ sortBy (comparing snd) $ Map.toList m

sol1 m template = sol' 10 m template

sol2 m template = sol' 40 m template

stats xs = sort [ (head zs, length zs) | zs <- ys ]
  where ys = group $ sort xs

main = do (template,rules) <- input1
          print (sol1 rules template)
          print (sol2 rules template)