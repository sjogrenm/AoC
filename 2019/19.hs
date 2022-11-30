import Data.Maybe
import Data.Ord
import Data.List
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Tree

import Util
import qualified IntCode
import qualified IntCodeM

input1 :: IO [Int]
input1 = do [ls] <- lines `fmap` readFile "19.txt"
            return $ map read $ splitOn ',' ls

--sol1 p = fst $ IntCodeM.runIntCode (V.fromList p) []

gen p a b = [ [ if x <= y && 2*x >= y `div` 3 && head ks == 1 then '#' else '.' | x <- [a..b], let ks = IntCode.runIO p [x,y] ]
            | y <- [a..b]
            ]

sol1 p n = length [ () | y <- [0..n-1], x <- [0..n-1],
                       let [k] = IntCode.runIO p [x,y], k == 1 ]



sol2 p = sol2' 0 100
  where sol2' x y | not (inBeam x y)            = sol2' (x+1) y
                  | inBeam (x+99) (y-99)        = 10000*x + (y-99)
                  | otherwise                   = sol2' x (y+1)
        inBeam x y = IntCode.runIO p [x,y] == [1]

traceIt x = trace (show x) x

triples [] = []
triples (a:b:c:zs) = ((a,b),c) : triples zs

main = do i <- input1
          let p = UV.fromList (i ++ replicate 1000 0)
          --mapM_ putStrLn $ gen p 100000
          --print $ sol1 i 50
          print $ sol2 p
