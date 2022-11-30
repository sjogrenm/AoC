{-# LANGUAGE FlexibleContexts #-}

import Data.Maybe
import Data.Ord
import Data.List
import Data.Char
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Data.Bits

import Util

input = parseInput id "24.txt"

data Op = Sym Char | Val Int | Add Op Op | Mul Op Op | Div Op Op | Mod Op Op | Eql Op Op
    deriving (Eq,Show)

minmax (Sym _) = Just (0,9)
minmax (Val x) = Just (x,x)
minmax (Add a b) = case (minmax a, minmax b) of
                     (Just x, Just y) -> Just (fst x + fst y, snd x + snd y)
                     _                -> Nothing
minmax (Mul a b) = case (minmax a, minmax b) of
                     (Just (x,y), Just (z,w)) | all (>=0) [x,y,z,w] -> Just (x*z,y*w)
                     _ -> Nothing
--minmax (Mul a b) = let x = minmax a ; y = minmax b in 
minmax (Mod a (Val y)) = case minmax a of
                           Just (b,c) | b >= 0 && c < y -> Just (b,c)
                           _                            -> Just (0,y-1)
minmax (Eql a b) = Just (0,1)
minmax _ = Nothing


-- a <= b <= c
-- d <= e <= f
--      b*e

go :: [String] -> [Op] -> Map.Map Char Op -> Map.Map Char Op
go [] _ regs = regs
go (x:xs) syms regs =
    case words x of
      ["inp", [c]]      -> go xs (tail syms) $ Map.insert c (head syms) regs
      ["add", [r], val] -> go xs syms $ Map.insert r (add (findReg r) (getVal val)) regs
      ["mul", [r], val] -> go xs syms $ Map.insert r (mul (findReg r) (getVal val)) regs
      ["div", [r], val] -> go xs syms $ Map.insert r (div' (findReg r) (getVal val)) regs
      ["mod", [r], val] -> go xs syms $ Map.insert r (mod' (findReg r) (getVal val)) regs
      ["eql", [r], val] -> go xs syms $ Map.insert r (eql (findReg r) (getVal val)) regs
  where findReg r = Map.findWithDefault (Val 0) r regs
        getVal val@(c:_) | c == '-' || isNumber c = Val (read val)
        getVal [r] = findReg r


add (Val x) (Val y) = Val (x + y)
add (Val 0) b = b
add a (Val 0) = a
add (Add (Val x) b) (Val y) = Add (Val (x+y)) b
add (Add a (Val x)) (Val y) = Add a (Val (x+y))
add (Val x) (Add (Val y) b) = Add (Val (x+y)) b
add (Val x) (Add a (Val y)) = Add (Val (x+y)) a
add a b = Add a b

mul (Val x) (Val y) = Val (x * y)
mul (Val 0) b = Val 0
mul a (Val 0) = Val 0
mul (Val 1) b = b
mul a (Val 1) = a
mul (Mul (Val x) b) (Val y) = Mul (Val (x*y)) b
mul (Mul a (Val x)) (Val y) = Mul a (Val (x*y))
mul (Val x) (Mul (Val y) b) = Mul (Val (x*y)) b
mul (Val x) (Mul a (Val y)) = Mul (Val (x*y)) a
-- x + (a * b) = x*a + x*b
--mul (Val x) (Add a b) = add (mul (Val x) a) (mul (Val x) b)
--mul (Add a b) (Val y) = add (mul a (Val y)) (mul b (Val y))
mul a b = Mul a b

div' (Val x) (Val y) = Val (x `div` y)
div' a (Val 1) = a
div' a b | a == b = Val 1
div' xx@(Add (Mul a (Val 26)) b) (Val 26) = case minmax b of
                                              Just (u,v) | u >= 0 && v < 26 -> a
                                              _ -> Div xx (Val 26)
div' a b = Div a b

mod' (Val x) (Val y) = Val (x `mod` y)
mod' (Add (Mul a (Val 26)) b) (Val 26) = mod' b (Val 26)
mod' a (Val y) = case minmax a of
                   Just (u,v) | u >= 0 && v < y -> a
                   _ -> Mod a (Val y)
mod' a b = Mod a b

eql (Val x) (Val y) = Val (if x == y then 1 else 0)
eql (Sym a) (Sym b) = if a == b then Val 1 else Eql (Sym a) (Sym b)
--eql (Sym a) (Val y) = if y < 0 || y > 9 then Val 0 else eql (Sym a) (Val y)
--eql (Val x) (Sym b) = if x < 0 || x > 9 then Val 0 else eql (Val x) (Sym b)
--  (a == b) == 0
-- Eql (Eql (Add (Mod (Add (Sym 'a') (Val 14)) (Val 26)) (Val 12)) (Sym 'b')) (Val 0)
eql (Add a b) (Add c d) | a == c        = eql b d
                        | a == d        = eql b c
                        | b == c        = eql a d
                        | b == d        = eql a c
eql a b | a == b = Val 1
eql a b = case (minmax a, minmax b) of
            (Just ma, Just mb) | snd ma < fst mb    -> Val 0
                               | snd mb < fst ma    -> Val 0
            _ -> Eql a b


sol1 i n = let m = go (take (18*n) i) (map Sym ['a'..]) Map.empty
            in m -- Map.findWithDefault (Val 0) 'z' m

main = do i <- input
          let v = sol1 i 4
--          let m = go (take (18*4) i) (map Sym ['a'..]) Map.empty
          --let m = go (take 72 i) (map Val [1,2,3,4,9,9,9,9,9]) Map.empty
--          let v = Map.findWithDefault (Val 0) 'z' m
          print v
          --mapM_ print $ Map.toList m


{-

z = 0

inp w (w1)
x = (z mod 26) + 13                 x > 9
x = x != w                          x = 1
z *= 25 * x + 1                     z = 0
z += (w + 14) * x                   z = (w1 + 14)


inp w (w2)
x = (z mod 26) + 12                 x > 9
x = x != w                          x = 1
z *= 25 * x + 1                     z = (w1+14)*26
z += (w + 8) * x                    z = (w1+14)*26 + (w2+8)


inp w (w3)
x = (z mod 26) + 11                 x = w2+8+11 = w2+19 > 9
x = x != w                          x = 1
z *= 25 * x + 1                     z = (w2+8)*26
z += (w + 5) * x                    z = (w2+8)*26 + (w3+5)


inp w (w4)
x = (z mod 26)                      x = w3+5
z = z div 26                        z = w2+8
x = x != w                          w2+3 = w3:  x = 0       else:   x = 1
z *= 25 * x + 1                                 z = w2+8            z = (w2+8)*26
z += (w + 4) * x                                z = w2+8            z = (w2+8)*26 + (w4+4)


inp w (w5)
x = (z mod 26) + 15                             x = w2+23           x = w4+19
x = x != w                                      x = 1               x = 1
z *= 25 * x + 1                                 z = (w2+23)*26      z = (w4+19)*26
z += (w + 10) * x                               z += (w5+10)


inp w (w6)
x = (z mod 26) - 13                             x = w5-3            x = w5-3
z = z div 26                                    z = w2+23           z = w4+19
x = x != w
z *= 25 * x + 1
z += (w + 13) * x
-}