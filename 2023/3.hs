import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import Util


input = lines `fmap` readFile "3.txt"

data Thing = Num { value :: Int, row :: Int, colStart :: Int, colEnd :: Int }
           | Sym { symbol :: Char, row :: Int, col :: Int }
           deriving Show

findThings input = numbers' input [0..]

numbers' (i:is) (r:rs) = numbers'' i r [0..] ++ numbers' is rs
numbers' [] _          = []

numbers'' ('.':is) r (_:cs)                 = numbers'' is r cs
numbers'' (i:is) r (c:cs) | isDigit i       = Num (read val) r c (c + length val - 1) : numbers'' rest r (drop (length val) (c:cs))
  where (val,rest) = span isDigit (i:is)
numbers'' (i:is) r (c:cs)                   = Sym i r c : numbers'' is r cs
numbers'' [] _ _                            = []


sol1 input = sum numbers
  where things = findThings input
        numbers = [ val | num@(Num val _ _ _) <- things, hasSym num ]
        hasSym (Num _ r cs ce) = not $ null [ sym | sym@(Sym _ r' c') <- things, r' `elem` [r-1..r+1] && c' `elem` [cs-1..ce+1] ]

sol2 input = sum [ a * b | (g, [Num a _ _ _, Num b _ _ _]) <- gears ]
  where things = findThings input
        gears = [ (g, ns) | g@(Sym '*' _ _) <- things, let ns = adjNumbers g, length ns == 2 ]
        adjNumbers (Sym _ r c) = [ num | num@(Num _ r' cs' ce') <- things, r `elem` [r'-1..r'+1] && c `elem` [cs'-1..ce'+1] ]

main = do i <- input
        --   print (findThings i)
        --   print i
          print (sol1 i)
        --   j <- input2
          print (sol2 i)
