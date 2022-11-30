module Foo where

import Data.List
import Data.Char
import Data.Graph
import Debug.Trace
import Data.Array

input = 
 [ "cpy 1 a"
 , "cpy 1 b"
 , "cpy 26 d"
 , "jnz c 2"
 , "jnz 1 5"
 , "cpy 7 c"
 , "inc d"
 , "dec c"
 , "jnz c -2"
 , "cpy a c"
 , "inc a"
 , "dec b"
 , "jnz b -2"
 , "cpy c b"
 , "dec d"
 , "jnz d -6"
 , "cpy 17 c"
 , "cpy 18 d"
 , "inc a"
 , "dec d"
 , "jnz d -2"
 , "dec c"
 , "jnz c -5"
 ]

testinput =
 [ "cpy 41 a"
 , "inc a"
 , "inc a"
 , "dec a"
 , "jnz a 2"
 , "dec a"
 ]

newtype Reg = Reg Char
        deriving (Eq,Show)

data Instruction = CpyReg Reg Reg | CpyLit Int Reg | JnzReg Reg Int | JnzLit Int Int | Inc Reg | Dec Reg
        deriving (Eq,Show)

type Code = Array Int Instruction

isReg [x] = isAlpha x
isReg _   = False

mkReg [x] = Reg x

parse = parse' . words
  where parse' ["cpy",src,dest] | isReg src     = CpyReg (mkReg src) (mkReg dest)
                                | otherwise     = CpyLit (read src) (mkReg dest)
        parse' ["inc",reg]              = Inc (mkReg reg)
        parse' ["dec",reg]              = Dec (mkReg reg)
        parse' ["jnz",reg,dest] | isReg reg     = JnzReg (mkReg reg) (read dest)
                                | otherwise     = JnzLit (read reg) (read dest)

toArray xs = listArray (0,length xs - 1) xs

testdata = toArray (map parse testinput)
mydata = toArray (map parse input)

eval instructions = eval' (listArray ('a','d') [0,0,1,0]) 0 instructions

eval' regs ip instructions
  | ip > snd (bounds instructions)    = regs
  | otherwise  = case instructions ! ip of
                   CpyReg (Reg src) (Reg dest) -> eval' (regs // [(dest, regs ! src)]) (ip+1) instructions
                   CpyLit val       (Reg dest) -> eval' (regs // [(dest, val)]) (ip+1) instructions
                   Inc (Reg r) -> eval' (regs // [(r, (regs ! r) + 1)]) (ip+1) instructions
                   Dec (Reg r) -> eval' (regs // [(r, (regs ! r) - 1)]) (ip+1) instructions
                   JnzReg (Reg r) offset -> let offset' = if regs ! r == 0 then 1 else offset
                                             in eval' regs (ip+offset') instructions
                   JnzLit val     offset -> let offset' = if val == 0 then 1 else offset
                                             in eval' regs (ip+offset') instructions
