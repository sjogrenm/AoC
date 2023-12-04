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


input = parseInput f "2.txt"

--f :: String -> (Int, [String])
f ('G':'a':'m':'e':' ':s) =
    case splitOn ':' s of
      [id, xs] -> (read id :: Int, map g $ splitOn ';' xs)

g :: String -> [(String, Int)]
g s = [ (col,read cnt) | ["",cnt,col] <- map (splitOn ' ') xs ]
  where xs = splitOn ',' s

sol1 xs = sum
    [ id
    | (id, game) <- xs,
      and [ r <= 12 && g <= 13 && b <= 14
          | draw <- game,
            let r = findOr 0 "red" draw
                g = findOr 0 "green" draw
                b = findOr 0 "blue" draw ]
    ]

findOr deflt a xs = case lookup a xs of
                      Nothing -> deflt
                      Just x  -> x


sol2 xs = sum
    [ r * g * b
    | (_, game) <- xs,
      let draws = concat game
          r = mins "red" draws 0
          g = mins "green" draws 0
          b = mins "blue" draws 0
    ]

mins col [] acc = acc
mins col ((c,n):xs) acc | col == c      = mins col xs (max n acc)
mins col (_:xs) acc = mins col xs acc

main = do i <- input
        --   print i
          print (sol1 i)
        --   j <- input2
          print (sol2 i)
