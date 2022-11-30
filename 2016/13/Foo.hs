module Foo where

import Data.List
import Data.Char
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.BFS (esp)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Tree
import Debug.Trace
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map ((!))
import Data.Bits (popCount)


isRoom d (x,y) = even (popCount foo)
  where foo = x*x + 3*x + 2*x*y + y + y*y + d


printRooms d width height = sequence_ [ putStrLn [ if (x,y) == (31,39) then 'O' else if isRoom d (x,y) then '.' else '#' | x <- [0..width] ] | y <- [0..height] ]


graph :: Int -> Int -> Int -> Gr () ()
graph d width height = mkGraph ns es
  where ns = [ (c2n (x,y), ()) | x <- [0..width], y <- [0..height], isRoom d (x,y) ]
        es = concatMap (horizExits d width) [0..height] ++ concatMap (vertExits d height) [0..width]

horizExits d width y = concat [ [ (c2n c1, c2n c2, ()), (c2n c2, c2n c1, ()) ]
                              | x <- [0..width-1], let c1 = (x,y), let c2 = (x+1,y), isRoom d c1 && isRoom d c2 ]
vertExits d height x = concat [ [ (c2n c1, c2n c2, ()), (c2n c2, c2n c1, ()) ]
                              | y <- [0..height-1], let c1 = (x,y), let c2 = (x,y+1), isRoom d c1 && isRoom d c2 ]

n2c n = n `divMod` 10000
c2n (x,y) = x*10000 + y

result1 = tail $ esp (c2n (1,1)) (c2n (31,39)) $ graph 1350 100 75

result2 = undefined


bounded_dfs [] _ = []
bounded_dfs _ g | isEmpty g = []

bounded_dfs ((b,v):vs) g = case match v g of
                             (Just c, g') -> (if b > 0 then [node' c] else []) ++ bounded_dfs ([ (b-1,n) | n <- suc' c ]++vs) g'
                             (Nothing, g') -> bounded_dfs vs g'
--xdfsWith suc' node' vs g

bounded_bfs_ q g | queueEmpty q || isEmpty g = []
bounded_bfs_ q g = case match v g of
                     (Just c, g') -> (if b > 0 then [node' c] else []) ++ bounded_bfs_ (queuePutList [(b-1,n) | n <- suc' c] q') g'
                     (Nothing, g') -> bounded_bfs_ q' g'
  where ((b,v),q') = queueGet q

bounded_bfs b n g = bounded_bfs_ (queuePut (b,n) mkQueue) g


data Queue a = MkQueue [a] [a]

mkQueue :: Queue a
mkQueue = MkQueue [] []

queuePut :: a -> Queue a -> Queue a
queuePut item (MkQueue ins outs) = MkQueue (item:ins) outs

queuePutList :: [a] -> Queue a -> Queue a
queuePutList xs q = foldl' (flip queuePut) q xs

queueGet :: Queue a -> (a, Queue a)
queueGet (MkQueue ins (item:rest)) = (item, MkQueue ins rest)
queueGet (MkQueue ins []) = queueGet (MkQueue [] (reverse ins))

queueEmpty :: Queue a -> Bool
queueEmpty (MkQueue ins outs) = null ins && null outs
