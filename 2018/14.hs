import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
--import Data.Array
import Debug.Trace (trace, traceShow)
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as Map
import Control.Parallel.Strategies

import Util


input :: Int
input = 652601

sol1 i = sol1' i 0 1 2 [7,3]

sol1' i e1 e2 len curr | len == i+10      = reverse $ take 10 curr
                       | len > i+10       = error "oh dear"
                       | otherwise        = sol1' i e1' e2' len' {- $ traceShow (e1,e2,r1,r2,e1',e2',len',reverse (rs++curr)) $ -} (rs++curr)
  where r1 = curr !! (len - e1 - 1)
        r2 = curr !! (len - e2 - 1)
        rr = r1 + r2
        rs | rr >= 10   = [rr `mod` 10, rr `div` 10]
           | otherwise  = [rr]
        len' = len + length rs
        e1' = (e1 + 1 + r1) `mod` len'
        e2' = (e2 + 1 + r2) `mod` len'

sol1M i = V.take 10 $ V.drop i $ V.create $
        do v <- MV.replicate (input + 20) (-1)
           let f v len e1 e2 | len == i+10    = return v
                             | len > i+10     = fail "oh dear"
                             | otherwise = do r1 <- MV.read v e1
                                              r2 <- MV.read v e2
                                              let rr = r1+r2
                                              len' <- addNew v len rr
                                              f v len' ((e1 + r1 + 1) `mod` len') ((e2 + r2 + 1) `mod` len')
           MV.write v 0 3
           MV.write v 1 7
           f v 2 0 1

addNew v oldLen rr
  | rr >= 10  = do MV.write v (oldLen) (rr `div` 10)
                   MV.write v (oldLen + 1) (rr `mod` 10)
                   return (oldLen + 2)
  | otherwise = do MV.write v (oldLen) rr
                   return (oldLen + 1)

display v = V.foldr (\x xs -> show x ++ xs) "" v


sol2M i = runST $
        do v <- MV.replicate (400 * input) (-1)
           let f v len e1 e2 = do done1 <- checkTail is v (len-1)
                                  done2 <- checkTail is v len
                                  case (done1, done2) of
                                    (True, _)     -> return (len-1 - length is)
                                    (False, True) -> return (len - length is)
                                    otherwise ->
                                         do r1 <- MV.read v e1
                                            r2 <- MV.read v e2
                                            let rr = r1+r2
                                            len' <- addNew v len rr
                                            f v len' ((e1 + r1 + 1) `mod` len') ((e2 + r2 + 1) `mod` len')
           MV.write v 0 3
           MV.write v 1 7
           f v 2 0 1
  where is = inputAsList i

inputAsList 0 = []
inputAsList n = inputAsList (n`div`10) ++ [n`mod`10]

checkTail is v len
  | len < isLen  = return False
  | otherwise    = let v' = MV.slice (len - isLen) isLen v
                    in do foo <- sequence [ MV.read v' k | (_,k) <- zip is [0..] ]
                          return $ foo == is
  where isLen = length is


main = do let r = sol1M input
          putStrLn $ display r
          print (sol2M input)
