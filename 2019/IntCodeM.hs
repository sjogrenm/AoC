module IntCodeM where

import Debug.Trace (trace)
import Control.Monad.ST (runST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Util


runIntCode :: V.Vector Int -> [Int] -> ([Int], V.Vector Int)
runIntCode xs input = runST $
        do v <- V.thaw (V.concat [xs, V.replicate 10000000 0])
           output <- impl v 0 0 input
           v' <- V.freeze v
           return (output, v')

getGrowth len newLen | len > newLen       = len - newLen
                     | otherwise          = getGrowth (2*len) newLen

impl v pos rb input = impl' pos rb input
  where impl' pos rb input = do
            (flags,opcode) <- fmap (flip divMod 100) $ MV.read v pos
            let params = reverse (0:0:0:0:digits flags)
--                read (-1) = throw (IndexOutOfBounds "-1")
                read loc = if loc >= MV.length v
                             then return 0
                             else MV.read v loc
--                write loc val if loc >= MV.length v
--                                then MV.grow v (getGrowth (MV.length v) loc)
                index ix = traceIf (pos+ix+1<0) ("index " ++ show ix) $ do
                              a <- read (pos+ix+1)
                              return $ case params !! ix of
                                         0 -> a
                                         1 -> a
                                         2 -> a + rb
                value ix = traceIf (pos+ix+1<0) ("value " ++ show ix) $ do 
                              a <- read (pos+ix+1)
                              case params !! ix of
                                0 -> traceIf (a<0) ("param 0") $ read a
                                1 -> return a
                                2 -> traceIf (a+rb<0) ("param 2") $ read (a + rb)
            i1 <- index 0
            i2 <- index 1
            i3 <- index 2
            v1 <- value 0
            v2 <- value 1
            case opcode of
              1 -> MV.write v i3 (v1+v2)            >> impl' (pos+4) rb input
              2 -> MV.write v i3 (v1*v2)            >> impl' (pos+4) rb input
              3 -> MV.write v i1 (head input)       >> impl' (pos+2) rb (tail input)
              4 -> do output <- impl' (pos+2) rb input
                      return (v1 : output)
              5 -> impl' (if v1 /= 0 then v2 else pos+3) rb input
              6 -> impl' (if v1 == 0 then v2 else pos+3) rb input
              7 -> MV.write v i3 (if v1 < v2 then 1 else 0)     >> impl' (pos+4) rb input
              8 -> MV.write v i3 (if v1 == v2 then 1 else 0)    >> impl' (pos+4) rb input
              9 -> impl' (pos+2) (rb+v1) input
              99 -> return []

traceIf True s = trace s
traceIf False _ = id
