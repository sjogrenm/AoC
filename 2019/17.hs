import Data.Char (chr, ord)
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
--import qualified IntCodeM

input1 :: IO [Int]
input1 = do [ls] <- lines `fmap` readFile "17.txt"
            return $ map read $ splitOn ',' ls

sol1 p = sum [ x * y
             | (x,y) <- Set.elems set
             , (x-1,y) `Set.member` set
             , (x+1,y) `Set.member` set
             , (x,y-1) `Set.member` set
             , (x,y+1) `Set.member` set ]
  where ls = lines $ map chr $ IntCode.runIO (UV.fromList p) []
        set = Set.fromList [ (x,y) | (y,ws) <- zip [0..] ls, (x,'#') <- zip [0..] ws ]


sol2 p = IntCode.runIO (UV.fromList p) $ progMain ++ progA ++ progB ++ progC ++ videoFeed
  where progMain = map ord "A,A,B,C,B,C,B,C,B,A\n"
        progA = map ord "R,6,L,12,R,6\n"
        progB = map ord "L,12,R,6,L,8,L,12\n"
        progC = map ord "R,12,L,10,L,10\n"
        videoFeed = map ord "n\n"

main = do i <- input1
          --print $ sol1 i
          print $ sol2 (2 : tail i)

input2 = "R 6 L 12 R 6 R 6 L 12 R 6 L 12 R 6 L 8 L 12 R 12 L 10 L 10 L 12 R 6 L 8 L 12 R 12 L 10 L 10 L 12 R 6 L 8 L 12 R 12 L 10 L 10 L 12 R 6 L 8 L 12 R 6 L 12 R 6 "

input2' = progA ++ progA ++ progB ++ progC ++ progB ++ progC ++ progB ++ progC ++ progB ++ progA


progA = "R 6 L 12 R 6 "
progB = "L 12 R 6 L 8 L 12 "
progC = "R 12 L 10 L 10 "
