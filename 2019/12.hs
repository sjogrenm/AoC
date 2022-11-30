import Data.Maybe
import Data.Ord
import Data.List
import qualified Data.Vector as V
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Tree

import Util
import IntCode
import qualified IntCodeM

newtype Pos = P (Int,Int,Int) deriving (Show,Eq)
newtype Vel = V (Int,Int,Int) deriving (Show,Eq)

still = V (0,0,0)

(a,b,c) `add` (d,e,f) = (a+d,b+e,c+f)

sumAbs (a,b,c) = abs a + abs b + abs c

input1 = [P (1, -4, 3), P (-14, 9, -4), P (-4, -6, 7), P (6, -9, -11)]

example1 = [P (-1, 0, 2), P (2, -10, -7), P (4, -8, 8), P (3, 5, -1)]
example2 = [P (-8, -10, 0), P (5, 5, 10), P (2, -7, 3), P (9, -8, -3)]

addVel :: Vel -> Vel -> Vel
addVel (V a) (V b) = V (a `add` b)

addGravity :: Pos -> Pos -> Vel
addGravity (P (a,b,c)) (P (d,e,f)) = V (signum (d-a), signum (e-b), signum (f-c))

gravity :: [Pos] -> [Vel]
gravity ps = [ foldr1 addVel (map (addGravity p) ps) | p <- ps ]

gravity1 :: [Int] -> [Int]
gravity1 pxs = [ sum [ signum (k-px) | k <- pxs ] | px <- pxs ]

move (P p) (V v) = P (p `add` v)

energy (P p) (V v) = sumAbs p * sumAbs v

iter :: ([Pos],[Vel]) -> ([Pos],[Vel])
iter (ps,vs) = (ps', vs')
  where vs' = zipWith addVel vs (gravity ps)
        ps' = zipWith move ps vs'

sol1 n ps = sum $ zipWith energy ps' vs'
  where (ps',vs') = iterate iter (ps, repeat still) !! n


iter2 :: ([Int], [Int]) -> ([Int],[Int])
iter2 (pxs, vxs) = (pxs', vxs')
  where vxs' = zipWith (+) vxs (gravity1 pxs)
        pxs' = zipWith (+) pxs vxs'

sol2' pxs = [ k | (k,(pz,[0,0,0,0])) <- zip [0..] (iterate iter2 (pxs,[0,0,0,0])), pz == pxs ]

sol2 ps = lcm (lcm xPeriod yPeriod) zPeriod
  where xPeriod = sol2' [ a | P (a,_,_) <- ps ] !! 1
        yPeriod = sol2' [ b | P (_,b,_) <- ps ] !! 1
        zPeriod = sol2' [ c | P (_,_,c) <- ps ] !! 1

main = do --print $ sol1 10 example1
          print $ sol1 1000 input1
          --print $ sol2 example1
          --print $ sol2 example2
          print $ sol2 input1
