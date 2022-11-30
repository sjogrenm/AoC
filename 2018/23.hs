import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
import Data.Array
import Debug.Trace (trace, traceShow)
import qualified Data.Vector as V
--import Data.Vector ((!))
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies
import Data.Graph as G
import Data.Bits
import Text.Parsec
import Text.Parsec.Char

import Util


getMyInput = parseInput (fromRight . runParser p () "stuff") "23.txt"

p = do string "pos=<"
       px <- integer
       char ','
       py <- integer
       char ','
       pz <- integer
       string ">, r="
       r <- integer
       return ([px,py,pz],r)

integer = do x <- optionMaybe (char '-')
             xs <- many1 digit
             let result = (read $ maybe "" (:[]) x ++ xs) :: Int
             return result


aa `manhattan` bb = sum [ abs (a - b) | (a,b) <- zip aa bb ]

inRangeCount :: ([Int],Int) -> [[Int]] -> Int
inRangeCount (pos,r) xs = length [ x | x <- xs, pos `manhattan` x <= r ]


allInRange xs = [ inRangeCount x poss | x <- xs ]
  where poss = map fst xs


example :: [([Int],Int)]
example = [ ([0,0,0], 4)
          , ([1,0,0], 1)
          , ([4,0,0], 3)
          , ([0,2,0], 1)
          , ([0,5,0], 3)
          , ([0,0,3], 1)
          , ([1,1,1], 1)
          , ([1,1,2], 1)
          , ([1,3,1], 1)
          ]

sol1 :: [([Int], Int)] -> Int
sol1 xs = inRangeCount x poss
  where x = maximumBy (comparing snd) xs
        poss = map fst xs

main = do i <- getMyInput
          print (sol1 i)
