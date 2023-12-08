import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Util

input = do s <- readFile "8.txt"
           let (lr:_:nodes) = lines s
           return (lr, map f nodes)

f :: String -> (String, (String, String))
f s = fromRight $ runParser p () "8.txt" s
  where p = do f <- many1 upper
               spaces
               char '='
               spaces
               char '('
               tl <- many1 upper
               char ','
               spaces
               tr <- many1 upper
               char ')'
               return (f, (tl, tr))


sol1 (lr, nodes) = sol1' (cycle lr) m 0 "AAA"
  where m = Map.fromList nodes

sol1' _ _ k "ZZZ" = k
sol1' (i:lr) m k node = sol1' lr m (k + 1) (iter m i node)

iter m i node = case i of
                    'L' -> l
                    'R' -> r
  where Just (l,r) = Map.lookup node m


sol2 (lr, nodes) = listlcm $ map (sol2' (cycle lr) m 0) [ n | (n,_) <- nodes, last n == 'A' ]
  where m = Map.fromList nodes

sol2' _ _ k node | last node == 'Z' = k
sol2' (i:lr) m k node = sol2' lr m (k + 1) (iter m i node)

main = do
        i <- input
        -- print i
        print (sol1 i)
        print (sol2 i)
        return ()

