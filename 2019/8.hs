import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Tree

import Util
import IntCode

input1 :: IO [Int]
input1 = pf `fmap` readFile "8.txt"

pf xs = [ read [x] | x <- xs, x `elem` "0123456789" ]

layers ls [] = []
layers ls xs = let (a,b) = splitAt ls xs in a : layers ls b

sol1 ls = count 1 l * count 2 l
  where count k xs = length $ filter (==k) xs
        l = head $ sortBy (comparing $ count 0) ls

sol2 ls = map color ts
  where ts = transpose ls
        color (2:cs) = color cs
        color (c:cs) = c

printImg cols xs = mapM_ (putStrLn . map p) $ layers cols xs
  where p 0 = ' '
        p 1 = 'X'

main = do i <- input1
          let ls = layers (25*6) i
          --print $ length ls
          print $ sol1 ls
          --printImg 2 $ sol2 [[0,2,2,2], [1,1,2,2], [2,2,1,2], [0,0,0,0]]
          printImg 25 $ sol2 ls

