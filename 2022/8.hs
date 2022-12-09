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

input = do cont <- readFile "8.txt"
           let ls = lines cont
           return (r ls)

r ls = [ [ read [x] :: Int | x <- l ] | l <- ls ]

ex = r [ "30373"
       , "25512"
       , "65332"
       , "33549"
       , "35390" ]

visit :: Ord a => a -> (a -> a) -> Int -> [Int] -> Set.Set a -> Set.Set a
visit pos inc m (b:cs) visible
  | m < b           = visit (inc pos) inc b         cs (Set.insert pos visible)
  | otherwise       = visit (inc pos) inc (max m b) cs visible
visit _ _ _ _ visible = visible

tailAndInit (x:xs) = init xs

incX (x,y) = (x+1,y)
decX (x,y) = (x-1,y)
incY (x,y) = (x,y+1)
decY (x,y) = (x,y-1)

sol1 i = Set.size $ Set.unions $
         [ visit (1,y)             incX l ls Set.empty | (l:ls,y) <- tailAndInit (zip i [0..]) ]
      ++ [ visit (length ls - 2,y) decX k ks Set.empty | (ls,y) <- tailAndInit (zip i [0..]), let k:ks = reverse ls ]
      ++ [ visit (x,1)             incY l ls Set.empty | (l:ls,x) <- tailAndInit (zip tr [0..]) ]
      ++ [ visit (x,length ls - 2) decY k ks Set.empty | (ls,x) <- tailAndInit (zip tr [0..]), let k:ks = reverse ls ]
      ++ [given]
  where tr = transpose i
        maxX = length (head i) - 1
        maxY = length i - 1
        given = Set.fromList $ concat [ [ (0,y), (maxX,y) ] | y <- [0..maxY] ] ++ concat [ [ (x,0), (x,maxY) ] | x <- [0..maxX ] ]



sol2 i = maximum [ score pos | pos <- Map.keys m ]
  where m = Map.fromList [ ((x,y),v) | (vs,y) <- zip i [0..], (v,x) <- zip vs [0..] ]
        score pos = product [sight pos incX, sight pos decX, sight pos incY, sight pos decY]
        sight pos inc = sight' (inc pos)
          where Just orig = Map.lookup pos m
                sight' pos' =
                    case Map.lookup pos' m of
                      Just t -> 1 + (if t >= orig then 0 else sight' (inc pos'))
                      Nothing -> 0
  

main = do i <- input
        --   print (head i)
          print (sol1 i)
          print (sol2 i)
