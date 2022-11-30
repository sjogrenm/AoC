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

parseBoard :: [String] -> [[Int]]
parseBoard b = [ map read ws | r <- b, let ws = words r ]

mkEditableBoard b = [ [ (c,False) | c <- r ] | r <- b ]

input1 = do cs <- readFile "4.txt"
            let l:ls = lines cs
                nums = map read (splitOn ',' l) :: [Int]
                boards = map parseBoard $ drop 1 $ splitOn "" ls
            return (nums, boards)

winning b = any p b || any p (transpose b)
  where p r = all snd r

score b = sum [ n | r <- b, (n,False) <- r ]

mark n b = [ [ (k, p || n == k) | (k,p) <- r ] | r <- b ]

sol1 (n:ns) bs = case wins of
                   [] -> sol1 ns bs'
                   [sc] -> n * sc
  where bs' = map (mark n) bs
        wins = [ score b | b <- bs', winning b ]

sol2 (n:ns) bs = case bs' of
                   [b] | winning b -> n * score b
                   _ -> sol2 ns bs''
  where bs' = map (mark n) bs
        bs'' = [ b | b <- bs', not (winning b) ]

main = do (nums,boards) <- input1
          let boards' = map mkEditableBoard boards
          print $ sol1 nums boards'
          print $ sol2 nums boards'
