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
input1 = do [ls] <- lines `fmap` readFile "13.txt"
            return $ map read $ splitOn ',' ls

--sol1 p = fst $ IntCodeM.runIntCode (V.fromList p) []
sol1 p = length $ filter (==2) $ Map.elems $ Map.fromList $ triples $ IntCode.runIO (UV.fromList p) []

sol2 p = sol2'  0 (UV.fromList (p ++ replicate 10000 0)) 0 0 [] 0 0
  where sol2' score p pos rb input bx px
          = case IntCode.runIO3 p pos rb input of
              IntCode.AwaitingInput p pos rb output ->
                let bx' = case [ x | ((x,_),4) <- triples output ] of
                            [] -> bx
                            [x] -> x
                    px' = case [ x | ((x,_),3) <- triples output ] of
                            [] -> px
                            [x] -> x
                 in do --putStr "Out: "; print output
                       --putStr "In: " ; print $ instruction bx' px'
                       sol2' (findScore output) p pos rb [instruction bx' px'] bx' px'
              IntCode.Done _ output -> return $ findScore output
          where findScore output = maximum (score : [ c | ((-1,0),c) <- triples output ])
        --instruction output = case ([ x | ((x,_),4) <- triples output ], [ x | ((x,_),3) <- triples output ]) of
        --                       ([bx],[px]) | bx < px    -> -1
        --                                   | bx > px    -> 1
        --                       otherwise                -> 0
        instruction bx px
          | bx < px     = -1
          | bx > px     = 1
          | otherwise   = 0

traceIt x = trace (show x) x

triples [] = []
triples (a:b:c:zs) = ((a,b),c) : triples zs

main = do i <- input1
          --print $ sol1 i
          --print $ 
          score <- sol2 (2 : drop 1 i)
          print score
