import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map

import Util

ex = ["L.LL.LL.LL"
     ,"LLLLLLL.LL"
     ,"L.L.L..L.."
     ,"LLLL.LL.LL"
     ,"L.LL.LL.LL"
     ,"L.LLLLL.LL"
     ,"..L.L....."
     ,"LLLLLLLLLL"
     ,"L.LLLLLL.L"
     ,"L.LLLLL.LL"]

input1 :: IO [String]
input1 = parseInput id "11.txt"

fudge is = [replicate n '.'] ++ [ ['.']++i++['.'] | i <- is ] ++ [replicate n '.']
  where n = length (head is) + 2

l2a i = mkArray (map mkArray i)

iter1 :: [String] -> [String]
iter1 i = [ [ case (a ! y) ! x of
                               '.' -> '.'
                               'L' | neighbours x y == 0 -> '#'
                                   | otherwise           -> 'L'
                               '#' | neighbours x y >= 4 -> 'L'
                                   | otherwise           -> '#'
                           | x <- indices (a ! y) ]
                 | y <- indices a ]
  where a = l2a i
        neighbours x y = length [ () | x' <- [x-1..x+1], y' <- [y-1..y+1], x' >= xMin, y' >= yMin, x' <= xMax, y' <= yMax, (x',y') /= (x,y), (a!y')!x' == '#' ]
        (yMin,yMax) = bounds a
        (xMin,xMax) = bounds (a ! yMin)

sol1 i = if i == i' then i else sol1 i'
  where i' = iter1 i


iter2 i = [ [ case (a ! y) ! x of
                               '.' -> '.'
                               'L' | countNeighbours x y == 0 -> '#'
                                   | otherwise           -> 'L'
                               '#' | countNeighbours x y >= 5 -> 'L'
                                   | otherwise           -> '#'
                           | x <- indices (a ! y) ]
                 | y <- indices a ]
  where a = l2a i
        countNeighbours x y = length (neighbours a x y)

sol2 i = if i == i' then i else sol2 i'
  where i' = iter2 i

neighbours a x y = [ head bs | as <- los a x y, let bs = [ (x',y',s) | (x',y') <- as, let s = a!y'!x', s /= '.' ], case bs of ((_,_,'#'):_) -> True; otherwise -> False ]

los a x y = [ [ (x',y ) | x' <- xR ],
              [ (x',y') | x' <- xR, y' <- yD, abs (x'-x) == abs (y'-y) ],
              [ (x, y') |           y' <- yD ],
              [ (x',y') | x' <- xL, y' <- yD, abs (x'-x) == abs (y'-y) ],
              [ (x',y ) | x' <- xL ],
              [ (x',y') | x' <- xL, y' <- yU, abs (x'-x) == abs (y'-y) ],
              [ (x, y') |           y' <- yU ],
              [ (x',y') | x' <- xR, y' <- yU, abs (x'-x) == abs (y'-y) ]
            ]
  where (yMin,yMax) = bounds a
        (xMin,xMax) = bounds (a ! yMin)
        xL = [x-1,x-2..xMin]
        xR = [x+1..xMax]
        yU = [y-1,y-2..yMin]
        yD = [y+1..yMax]


occuppied i = length [ () | y <- i, x <- y, x == '#' ]

main = do i <- input1
          mapM_ print i
          print (occuppied i)
          let s = sol1 i
          mapM_ print s
          print (occuppied s)
          let s2 = sol2 i
          mapM_ print s2
          print (occuppied s2)
        --   let e2 = iter ex
        --       e3 = iter e2
        --       e4 = iter e3
        --       e5 = iter e4
        --       e6 = iter e5
        --       e7 = iter e6
        --   putStrLn ""
        --   mapM_ print e7
        --   putStrLn ""
        --   mapM_ print (iter e7)
          --print (sol2 i)

