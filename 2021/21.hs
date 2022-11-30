{-# LANGUAGE FlexibleContexts #-}

import Data.Maybe
import Data.Ord
import Data.List
import Data.Char
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import Control.Monad.Memo
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Data.Bits

import Util

mod10 n = case n `mod` 10 of
            0 -> 10
            k -> k

play p1 s1 p2 s2 (d1:d2:d3:dice) = if s1' >= 1000 then s2 * d3 else play p2 s2 p1' s1' dice
  where p1' = mod10 (p1 + d1 + d2 + d3)
        s1' = s1 + p1'

ex1 = play 4 0 8 0 [1..]

sol1 = play 4 0 1 0 [1..]


data State = S Int Int Int Int
  deriving (Eq,Show,Ord)

dice = [ (length d,head d) | d <- group $ sort [ d1+d2+d3 | d1 <- [1..3], d2 <- [1..3], d3 <- [1..3] ]]

allStates = [ S p1 s1 p2 s2 | p1 <- [1..10], s1 <- [0..20], p2 <- [1..10], s2 <- [0..20] ]

nextState = Map.fromList [ ((st1,d),S p2 s2 p1' s1')
                         | st1@(S p1 s1 p2 s2) <- allStates, d <- [3,4,5,6,7,8,9],
                           let p1' = mod10 (p1 + d)
                               s1' = s1 + p1'
                         ]
--
--
--  f 3   -> 6, 7..7, 8..8, 9..9, 10..10, 1..1, 2
--

play2 (S p1 s1 p2 s2) = (sum w1s, sum w2s)
  where temp = [ if s1' >= 21 then (c,0) else (w1*c, w2*c)
               | (c,d) <- dice,
                 let p1' = mod10 (p1 + d)
                     s1' = s1 + p1'
                     (w2,w1) = play2 (S p2 s2 p1' s1')
               ]
        (w1s,w2s) = unzip temp
--        w1 = length (filter isNothing temp)
        --ss = [ s | Just s <- temp ]
        --(w2s,w1s) = unzip (map play2 ss)


play2m (S p1 s1 p2 s2) = do foo <- sequence [ let p1' = mod10 (p1 + d)
                                                  s1' = s1 + p1'
                                               in if s1' >= 21
                                                    then return (c,0)
                                                    else do (w2,w1) <- memo play2m (S p2 s2 p1' s1')
                                                            return (w1*c, w2*c)
                                            | (c,d) <- dice ]
                            let (w1s,w2s) = unzip foo
                            return (sum w1s, sum w2s)


ex2 = startEvalMemo $ play2m (S 4 0 8 0)

sol2 = let (s1,s2) = startEvalMemo $ play2m (S 4 0 1 0)
        in max s1 s2