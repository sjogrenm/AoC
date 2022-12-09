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
import Text.Parsec
import Text.Parsec.Expr

import Util

p1 = [30
     ,42
     ,25
     ,7
     ,29
     ,1
     ,16
     ,50
     ,11
     ,40
     ,4
     ,41
     ,3
     ,12
     ,8
     ,20
     ,32
     ,38
     ,31
     ,2
     ,44
     ,28
     ,33
     ,18
     ,10]

p2 = [36
     ,13
     ,46
     ,15
     ,27
     ,45
     ,5
     ,19
     ,39
     ,24
     ,14
     ,9
     ,17
     ,22
     ,37
     ,47
     ,43
     ,21
     ,6
     ,35
     ,23
     ,48
     ,34
     ,26
     ,49]

score q = sum (zipWith (*) (reverse (qGetList q)) [1..])

sol1 p1 p2 = score q
  where (winner,q) = iter1 (qPutList p1 mkQueue) (qPutList p2 mkQueue)

iter1 q1 q2 | qEmpty q1     = (2, q2)
            | qEmpty q2     = (1, q1)
            | otherwise     = let (a1,q1') = qGet q1
                                  (a2,q2') = qGet q2
                               in if a1 > a2 then iter1 (qPut a2 $ qPut a1 $ q1') q2' else iter1 q1' (qPut a1 $ qPut a2 $ q2')


sol2 p1 p2 = score q
  where (winner,q) = game Set.empty (qFromList p1) (qFromList p2)


game rounds q1 q2 | qEmpty q1 = (2, q2)
                  | qEmpty q2 = (1, q1)
                  | (q1,q2) `Set.member` rounds     = (1,q1)

game rounds q1 q2 = let (q1',q2') = gRound q1 q2
                     in game (Set.insert (q1,q2) rounds) q1' q2'
                      

gRound q1 q2 | c1 <= qSize q1' && c2 <= qSize q2'
                          = case game Set.empty (qFromList (take c1 $ qGetList q1')) (qFromList (take c2 $ qGetList q2')) of
                              (1,_) -> (qPut c2 $ qPut c1 $ q1', q2')
                              (2,_) -> (q1', qPut c1 $ qPut c2 $ q2')
             | c1 > c2               = (qPut c2 $ qPut c1 $ q1', q2')
             | c2 > c1               = (q1', qPut c1 $ qPut c2 $ q2')
  where (c1,q1') = qGet q1
        (c2,q2') = qGet q2


main = do --ts <- input1
          --mapM_ print ts
          --print (qPutList p1 mkQueue)
          print (sol1 p1 p2)
          print (sol2 p1 p2)
