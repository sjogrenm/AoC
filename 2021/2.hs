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

data Cmd = Forward Int | Dwn Int | Up Int
    deriving (Eq, Show)

input1 = parseInput r "2.txt"
  where r :: String -> Cmd
        r xs = case words xs of
                 ["forward", n] -> Forward (read n)
                 ["down", n] -> Dwn (read n)
                 ["up", n] -> Up (read n)

sol1 cs = let (hpos,depth) = sol1' (0,0) cs in hpos * depth
sol1' pos [] = pos
sol1' (hpos,depth) (c:cs) = case c of
    Forward n -> sol1' (hpos+n,depth) cs
    Dwn n     -> sol1' (hpos,depth+n) cs
    Up n      -> sol1' (hpos,depth-n) cs

sol2 cs = let (hpos,depth,aim) = sol2' (0,0,0) cs in hpos * depth
sol2' pos [] = pos
sol2' (hpos,depth,aim) (c:cs) = case c of
    Forward n -> sol2' (hpos+n,depth+n*aim,aim) cs
    Dwn n     -> sol2' (hpos,depth,aim+n) cs
    Up n      -> sol2' (hpos,depth,aim-n) cs

main = do i <- input1
          print (sol1 i)
          print (sol2 i)
