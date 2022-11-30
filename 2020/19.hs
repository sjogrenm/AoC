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
import Text.Parsec
import Text.Parsec.Expr

import Util

data RuleTarget = C Char | O [[Int]]
    deriving Show
type RuleSet = Map.IntMap RuleTarget

parseRule :: String -> (Int,RuleTarget)
parseRule xs = (read a,r)
  where [a,' ':b] = splitOn ':' xs
        ys = [ [ y | y <- splitOn ' ' z, y /= "" ] | z <- splitOn '|' b ]
        r = case ys of
              [[['"',f,'"']]] -> C f
              _         -> O (map (map read) ys)

input1 = do i <- parseInput id "19.txt"
            let [a,b] = splitOn "" i
            return (Map.fromList $ map parseRule a,b)

matches rules s = any (=="") $ m rule0 s
  where Just rule0 = Map.lookup 0 rules
        m r [] = []
        m (C c) (x:xs) | c == x     = [xs]
                       | otherwise  = []
        m (O rss) xs = concat [ m' [ rule | ri <- rs, let Just rule = Map.lookup ri rules ] xs | rs <- rss ]
        m' [] xs = [xs]
        m' (r:rs) xs = concatMap (m' rs) $ m r xs

ex1 = Map.fromList (map parseRule ["0: 1 2","1: \"a\"","2: 1 3 | 3 1","3: \"b\""])
ex2 = Map.fromList (map parseRule ["0: 4 1 5","1: 2 3 | 3 2","2: 4 4 | 5 5","3: 4 5 | 5 4","4: \"a\"","5: \"b\""])

sol1 rules input = length [ i | i <- input, matches rules i ]

main = do (a,b) <- input1
          --print a
          print $ sol1 a b
          let a' = Map.insert 11 (O [[42,31],[42,11,31]]) $ Map.insert 8 (O [[42],[42,8]]) a
          print $ sol1 a' b
