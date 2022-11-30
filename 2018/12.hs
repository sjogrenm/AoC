import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
--import Data.Array
import Debug.Trace (trace)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as Map
import Text.Parsec
import Text.Parsec.Char
import Control.Parallel.Strategies

import Util

initial = ".#####.##.#.##...#.#.###..#.#..#..#.....#..####.#.##.#######..#...##.#..#.#######...#.#.#..##..#.#.#"

transitions = map parseTrans
              [ "#..#. => ."
              , "##... => #"
              , "#.... => ."
              , "#...# => #"
              , "...#. => ."
              , ".#..# => #"
              , "#.#.# => ."
              , "..... => ."
              , "##.## => #"
              , "##.#. => #"
              , "###.. => #"
              , "#.##. => ."
              , "#.#.. => #"
              , "##..# => #"
              , "..#.# => #"
              , "..#.. => ."
              , ".##.. => ."
              , "...## => #"
              , "....# => ."
              , "#.### => #"
              , "#..## => #"
              , "..### => #"
              , "####. => #"
              , ".#.#. => #"
              , ".#### => ."
              , "###.# => #"
              , "##### => #"
              , ".#.## => ."
              , ".##.# => ."
              , ".###. => ."
              , "..##. => ."
              , ".#... => #" ]

data Transition = [Char] :-> Char
        deriving (Eq, Show)

parseTrans xs = case words xs of
                  [a, "=>", [b]] -> a :-> b


toBit '.' = 0
toBit '#' = 1

hc xs = m $ map toBit xs
m [] = 0
m (x:xs) = x + 2 * m xs

sortedTransitions = sortBy (comparing f) transitions
  where f (pat :-> _) = hc pat

trMap :: V.Vector Char
trMap = V.fromList [ r | (_ :-> r) <- sortedTransitions ]

{-

.#.#
  ^

.#.#..
  ^

#.#..
  ^

-}

keepFlower '.' = []
keepFlower '#' = "#"

{-
apply' _ [] = []
apply' _ [_,_] = []
apply' _ "..." = []
apply' _ "...." = []
apply' _ "....." = []

apply' p xs | length xs < 5 = apply' p (xs++"..")

apply' p@(pat :-> r) xs | pat == ys       = 
                        | otherwise       = 
  where (ys@[a,b,c,d,e],zs) = splitAt 5 xs
        rec@(f:g:h:_) = apply' p (drop 1 xs)
-}

tryApply :: Transition -> [Char] -> Maybe Char
tryApply (pat :-> r) xs = toMaybe (pat == xs) r

--apply :: [Transition] -> [Char] -> Char
--apply ts xs = head $ catMaybes [ tryApply t xs | t <- ts ]
apply xs = trMap ! (hc xs)

chunks [a,b,c,d] = [ [a,b,c,d,'.'], [b,c,d,'.','.'], [c,d,'.','.','.'] ]
chunks (a:b:c:d:e:xs) = [a,b,c,d,e] : chunks (b:c:d:e:xs)
chunks _ = []

--iter :: [Transition] -> State -> State
iter (st,n) = trim (ys, n-1)
  where xs = chunks ("..." ++ st)
        ys = map apply xs

-- ("..#.asdf", -3) -> ("#.asdf", -1)
trim (xs,n) = (dropTail rest, n + length es)
  where (es,rest) = span (=='.') xs
        dropTail "" = ""
        dropTail "." = ""
        dropTail ".." = ""
        dropTail "..." = ""
        dropTail "...." = ""
        dropTail (x:xs) = x : dropTail xs

sol k = sum [ k | ('#',k) <- zip st [n..] ]
  where (st,n) = iterate iter (initial,0) !! k

sol1 = sol 20

sol2 = sol 50000000000


main = print (sol 500000)

{-

5000            -> 310293
50000           -> 3100293
500000          -> 31000293
50000000000     -> 3100000000293

-}
