module IntCode where

import Prelude hiding ((++))

import Data.Vector.Unboxed
import qualified Data.Vector.Unboxed as V
import Debug.Trace

import Util

{-
run xs pos = let (ys,[]) = runIO xs pos [] [] in ys

runIO xs pos input output = 
  case opcode of
    1 -> runIO (xs // [(i3, v1+v2)]) (pos+4) input output
    2 -> runIO (xs // [(i3, v1*v2)]) (pos+4) input output
    3 -> runIO (xs // [(i1, head input)]) (pos+2) (tail input) output
    4 -> runIO xs                         (pos+2) input (v1:output)
    5 -> runIO xs (if v1 /= 0 then v2 else pos+3) input output
    6 -> runIO xs (if v1 == 0 then v2 else pos+3) input output
    7 -> runIO (xs // [(i3, if v1 < v2 then 1 else 0)]) (pos+4) input output
    8 -> runIO (xs // [(i3, if v1 == v2 then 1 else 0)]) (pos+4) input output
    99 -> (xs, reverse output)
  where (flags,opcode) = (xs ! pos) `divMod` 100
        params = reverse ([0,0,0,0,0] ++ digits flags)
        i1 = xs ! (pos+1)
        v1 = value 0
        i2 = xs ! (pos+2)
        v2 = value 1
        i3 = xs ! (pos+3)
        value ix = let a = xs ! (pos+ix+1)
                    in case params !! ix of
                         0 -> xs ! a
                         1 -> a

-}

data State = AwaitingInput { mem :: Vector Int, pos :: Int, rb :: Int, output :: [Int] }
           | Done { mem :: Vector Int, output :: [Int] }
        deriving Show

consOutput v (AwaitingInput mem pos rb output) = AwaitingInput mem pos rb (v:output)
consOutput v (Done mem output) = Done mem (v:output)

runIO xs input = let Done _ output = runIO3 (xs ++ V.replicate 100000 0) 0 0 input
                  in output

runIO2 xs input = let Done mem _ = runIO3 (xs ++ V.replicate 100000 0) 0 0 input
                   in mem

runIO3 :: Vector Int -> Int -> Int -> [Int] -> State
runIO3 xs pos rb input =
  case opcode of
    1 -> runIO3 (xs // [(i3, v1+v2)]) (pos+4) rb input
    2 -> runIO3 (xs // [(i3, v1*v2)]) (pos+4) rb input
    3 -> case input of
           []     -> AwaitingInput xs pos rb []
           (i:is) -> runIO3 (xs // [(i1, i)]) (pos+2) rb is
    --runIO3 (xs // [(i1, Prelude.head input)]) (pos+2) rb (Prelude.tail input)
    4 -> consOutput v1 (runIO3 xs (pos+2) rb input)
         --let (output, prog) = runIO3 xs (pos+2) rb input
         -- in (v1:output, prog)
    5 -> runIO3 xs (if v1 /= 0 then v2 else pos+3) rb input
    6 -> runIO3 xs (if v1 == 0 then v2 else pos+3) rb input
    7 -> runIO3 (xs // [(i3, if v1 < v2 then 1 else 0)]) (pos+4) rb input
    8 -> runIO3 (xs // [(i3, if v1 == v2 then 1 else 0)]) (pos+4) rb input
    9 -> runIO3 xs (pos+2) (rb + v1) input
    99 -> Done xs []
  where (flags,opcode) = (xs ! pos) `divMod` 100
        params = Prelude.reverse (0:0:0:0:digits flags)
        i1 = index 0
        v1 = value 0
        i2 = index 1
        v2 = value 1
        i3 = index 2
        index ix = let a = xs ! (pos+ix+1)
                    in case params !! ix of
                         0 -> a
                         1 -> a
                         2 -> a+rb
        value ix = let a = xs ! (pos+ix+1)
                    in case params !! ix of
                         0 -> xs ! a
                         1 -> a
                         2 -> xs ! (a+rb)
