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

data Instr = Noop | AddX Int
    deriving (Eq,Show)

input = parseInput r "10.txt"
  where r xs = case words xs of
                 ["noop"] -> Noop
                 ["addx",v] -> AddX (read v)

xvalue x (Noop:is) = x : xvalue x is
xvalue x (AddX v:is) = x : (x+v) : xvalue (x+v) is
xvalue x [] = []

sol1 is = sum [str 20, str 60, str 100, str 140, str 180, str 220]
  where xs = 1 : xvalue 1 is
        str n = n * (xs !! (n-1))

sol2 is = printCrt [ if abs (x - k) <= 1 then '#' else ' ' | (x,k) <- zip xs coords ]
  where xs = 1 : xvalue 1 is
        coords = concat $ repeat [0..39]

printCrt xs | length xs > 1     = let (ys,zs) = splitAt 40 xs
                                   in putStrLn ys >> printCrt zs
printCrt _ = return ()

main = do i <- input
        --   print (head i)
          print (sol1 i)
          sol2 i
