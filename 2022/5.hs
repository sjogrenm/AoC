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

input = do contents <- readFile "5.txt"
           let xs = lines contents
               [_,ys] = splitOn "" xs
           return (map p ys)
  where p :: String -> (Int,Int,Int)
        p s = let ["move",count,"from",src,"to",dest] = splitOn ' ' s
               in (read count, read src, read dest)

initial = mkArray ["","SLFZDBRH", "RZMBT", "SNHCLZ", "JFCS", "BZRWHGP", "TMNDGZJV", "QPSFWNLG", "RZM", "TRVGLCM"]

mv1 (c,s,d) a = a // [(s, y), (d, reverse x ++ a!d)]
  where (x,y) = splitAt c (a ! s)

sol1 xs = concat [ head' y | y <- elems $ foldl (flip mv1) initial xs ]
  where head' (c:_) = [c]
        head' _ = []

mv2 (c,s,d) a = a // [(s, y), (d, x ++ a!d)]
  where (x,y) = splitAt c (a ! s)

sol2 xs = concat [ head' y | y <- elems $ foldl (flip mv2) initial xs ]
  where head' (c:_) = [c]
        head' _ = []

main = do i <- input
        --   print (head i)
          print (sol1 i)
          print (sol2 i)
