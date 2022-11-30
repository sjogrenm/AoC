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

data Tile = T Int [[Char]]
    deriving Show

instance Eq Tile where
    T i _ == T j _ = i == j

input1 = do i <- parseInput id "20.txt"
            let xs = splitOn "" i
            return [ T id ys | ('T':'i':'l':'e':' ':n):ys <- xs, let id = read (init n) ]

edges (T _ xs) = e1 ++ map reverse e1
  where ys = transpose xs
        e1 = [head xs, last xs, head ys, last ys]

countTilesWithCommonEdges t ts = length [ t2 | t2 <- ts, t /= t2, length (edges t `intersect` edges t2) > 0 ]

sol1 ts = product [ id | T id _ <- corners ]
  where corners = [ t | t <- ts, countTilesWithCommonEdges t ts == 2 ]

main = do ts <- input1
          --mapM_ print ts
          print (sol1 ts)
          --let t@(T _ xs) = head ts
          --mapM_ print (edges t)
          --mapM_ print xs
          --putStrLn ""
          --mapM_ print (transpose xs)
          --print $ sol1 a b
          --let a' = Map.insert 11 (O [[42,31],[42,11,31]]) $ Map.insert 8 (O [[42],[42,8]]) a
          --print $ sol1 a' b
