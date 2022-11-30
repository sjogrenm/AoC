import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
--import Data.Array
import Debug.Trace (trace)
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as Map
import Text.Parsec
import Text.Parsec.Char
import Control.Parallel.Strategies

import Util

serial :: Int
serial = 9810

power s x y = h - 5
  where r = x + 10
        t = (r * y + s) * r
        h = (t `div` 100) `mod` 10

mp sz a = maximumBy (comparing thd3) [ (x,y,f x y) | x <- [1..300-sz+1], y <- [1..300-sz+1] ]
  where f x y = sum [ a ! (x'*301 + y') | x' <- [x..x+sz-1], y' <- [y..y+sz-1] ]

sol1 s = mp 3 (mkA s)

mkA :: Int -> V.Vector Int
mkA s = V.create $ do
        a <- MV.new (302*302)
        sequence_ [ MV.write a (x*301 + y) (power s x y) | x <- [1..300], y <- [1..300] ]
        return a

sol2 :: Int -> ((Int,Int,Int),Int)
sol2 s = maximumBy (comparing (thd3 . fst)) ([ (foo,n) | n <- [1..300], let foo = mp n a ] `using` parListChunk 10 rdeepseq)
  where a = mkA s

sol2' :: Int -> [((Int,Int,Int),Int)]
sol2' s = [ (foo,n) | n <- [1..300], let foo = mp n a ]
  where a = mkA s


foo :: ((Int,Int,Int),Int) -> [((Int,Int,Int),Int)] -> IO ()
foo _ [] = return ()
foo x@((x1,y1,c1),n1) ((y@((x2,y2,c2),n2)):ys) =
        if c2 > c1
          then do print (x2,y2,n2)
                  foo y ys
          else foo x ys

main = let x:xs = sol2' serial
        in foo x xs
