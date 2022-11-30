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

--input1 :: IO [String]
input1 = do parseInput id "10.txt"

inv '<' = '>'
inv '(' = ')'
inv '[' = ']'
inv '{' = '}'
inv _   = 'X'

score1 ')' = 3
score1 ']' = 57
score1 '}' = 1197
score1 '>' = 25137

corrupted xs = f xs []
  where f [] exp = Left exp
        f (x:xs) exp | x `elem` "<([{"      = f xs (inv x:exp)
                     | null exp             = Right x
                     | x == head exp        = f xs (tail exp)
                     | otherwise            = Right x


sol1 xs = sum [ score1 (fromRight jc) | ys <- xs, let jc = corrupted ys, isRight jc ]

sol2 xs = let scores = sort [ score2 (fromLeft jc) 0 | ys <- xs, let jc = corrupted ys, isLeft jc ]
           in scores !! (length scores `div` 2)

score2 []     acc = acc
score2 (c:cs) acc = score2 cs (acc*5 + fromJust (lookup c scores))
  where scores = [(')',1), (']',2), ('}',3), ('>',4)]

main = do is <- input1
          print (sol1 is)
          print (sol2 is)
