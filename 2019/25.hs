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

input1 :: IO [Int]
input1 = do [ls] <- lines `fmap` readFile "25.txt"
            return $ map read $ splitOn ',' ls


sol1 (mem,pos,rb) input =
  let state = IntCode.runIO3 mem pos rb input
   in do putStr (map chr (IntCode.output state))
         case state of
           IntCode.AwaitingInput mem2 pos2 rb2 _ ->
             do i <- getLine
                sol1 (mem2,pos2,rb2) (map ord i ++ [10])
           _ -> return ()

main = do i <- input1
          let p = UV.fromList (i ++ replicate 10000 0)
          sol1 (p,0,0) []

tr x = trace ("<"++show x++">") x
