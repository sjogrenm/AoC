import Data.Char (chr, ord)
import Data.Maybe
import Data.Ord
import Data.List
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import Debug.Trace (trace, traceShow)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Tree

import Util
import qualified IntCode
--import qualified IntCodeM


input1text
       = [ "##.##"
         , ".#.##"
         , "##..#"
         , "#.#.."
         , ".###."
         ]

input1 = Set.fromList [ (x,y) | (y,ys) <- zip [0..] input1text,
                                (x,'#') <- zip [0..] ys ]


iter s = Set.fromList [ (x,y) | x <- [0..4], y <- [0..4],
                                let hasBug = Set.member (x,y) s
                                    ns = neighbours (x,y) s,
                                if hasBug then ns == 1 else ns `elem` [1,2] ]

neighbours (x,y) s = length [ () | (x',y') <- xys, Set.member (x',y') s ]
  where xys = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

pretty s = [ [ if Set.member (x,y) s then '#' else '.' | x <- [0..4] ]
           | y <- [0..4]
           ]

biodiversity s = sum [ 2^z | z <- [0..24], let (y,x) = z `divMod` 5, Set.member (x,y) s ]

sol1 = sol1' input1 []

sol1' s seen | s `elem` seen    = biodiversity s
             | otherwise        = sol1' (iter s) (s:seen)
