import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import Util

input1 = parseInput r "2.txt"
  where r :: String -> (Int,Int,Char,String)
        r xs = let [a,' ':pwd] = splitOn ':' xs
                   [c,[ch]] = splitOn ' ' a
                   [mi,ma] = splitOn '-' c
                in (read mi, read ma, ch, pwd)

sol1 xs = length [ pwd | (mi,ma,ch,pwd) <- xs, let chc = count ch pwd, chc >= mi, chc <= ma ]


sol2 xs = length [ pwd | (p1,p2,ch,pwd) <- xs, let a = pwd !! (p1-1), let b = pwd !! (p2-1),
                         (a == ch || b == ch) && (a /= ch || b /= ch) ]


main = do i <- input1
          print (sol1 i)
          print (sol2 i)
