import Data.Maybe
import Data.Ord
import Data.List
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Tree

import Util
--import qualified IntCode
--import qualified IntCodeM

input1 :: IO [Int]
input1 = do [ls] <- lines `fmap` readFile "16.txt"
            return $ [ read [c] | c <- ls ]


pattern n = tail ps
  where ps = concat $ repeat (replicate n 0 ++ replicate n 1 ++ replicate n 0 ++ replicate n (-1))

disp = concatMap show

solIter xs = [ abs (sum [ a*b | (a,b) <- zip (pattern n) xs ]) `mod` 10 | n <- [1..length xs] ]

sol1 0 xs = disp $ take 8 xs
sol1 k xs = sol1 (k-1) (solIter xs)

sol2 o 0 xs = reverse $ take 8 $ drop (o - 8) xs
sol2 o k (x:xs) = sol2 o (k-1) ys
  where ys = x : [ (z + y) `mod` 10 | (z,y) <- zip xs ys ]

example1 :: [Int]
example1 = concat $ replicate 10000 [ read [c] | c <- "03036732577212944063491565474664" ]
e1offset = read $ disp $ take 7 example1 :: Int

main = do i <- input1
          --print i
          --print $ sol1 3 [1,2,3,4,5,6,7,8]
          --print $ sol1 100 i
          let i2 = (concat $ replicate 10000 i)
              offset = read $ disp $ take 7 i2 :: Int
          print $ disp $ sol2 (length example1 - e1offset) 100 (reverse example1)
          --print (length i2)
          --print offset
          --print (length i2 - offset)
          print $ disp $ sol2 (length i2 - offset) 100 (reverse i2)
