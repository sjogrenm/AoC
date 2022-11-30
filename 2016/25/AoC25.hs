module Main where

import Control.Monad.ST
import Data.List
import Data.Char
import Data.Graph
import Debug.Trace
import Data.Array
import Data.Array.IO

input =
 [ "cpy a d"
 , "cpy 7 c"
 , "cpy 365 b"
 , "inc d"
 , "dec b"
 , "jnz b -2"
 , "dec c"
 , "jnz c -5"
 , "cpy d a"
 , "jnz 0 0"
 , "cpy a b"
 , "cpy 0 a"
 , "cpy 2 c"
 , "jnz b 2"
 , "jnz 1 6"
 , "dec b"
 , "dec c"
 , "jnz c -4"
 , "inc a"
 , "jnz 1 -7"
 , "cpy 2 b"
 , "jnz c 2"
 , "jnz 1 4"
 , "dec b"
 , "dec c"
 , "jnz 1 -4"
 , "jnz 0 0"
 , "out b"
 , "jnz a -19"
 , "jnz 1 -21"
 ]

data Arg = Reg Char | Lit Int
        deriving (Eq,Show)

data Instruction = Cpy Arg Arg
                 | Jnz Arg Arg
                 | Inc Arg
                 | Dec Arg
                 | Toggle Arg
                 | Mul Arg Arg Arg
                 | Out Arg
        deriving (Eq,Show)

type Code = Array Int Instruction

isReg [x] = isAlpha x
isReg _   = False

mkReg [x] = Reg x
mkLit xs = Lit (read xs)

mkArg xs | isReg xs     = mkReg xs
         | otherwise    = mkLit xs

parse = parse' . words
  where parse' ["cpy",src,dest] = Cpy (mkArg src) (mkArg dest)
        parse' ["inc",reg]      = Inc (mkArg reg)
        parse' ["dec",reg]      = Dec (mkArg reg)
        parse' ["jnz",reg,dest] = Jnz (mkArg reg) (mkArg dest)
        parse' ["tgl",reg]      = Toggle (mkArg reg)
        parse' ["out",reg]      = Out (mkArg reg)

toArray xs = listArray (0,length xs - 1) xs

nop = Jnz (Lit 0) (Lit 0)

optimize :: [Instruction] -> [Instruction]
optimize (Cpy (Lit 0) a1 : Cpy b1 c1 : Inc a2 : Dec c2 : Jnz c3 (Lit (-2)) : Dec d1 : Jnz d2 (Lit (-5)) : is)
  | and [a1 == a2, c1 == c2, c2 == c3, d1 == d2 ]       = Mul b1 d1 a1 : replicate 6 nop ++ optimize is
optimize (i:is) = i : optimize is
optimize [] = []

eval a instructions = eval' (listArray ('a','d') [a,0,0,0]) 0 instructions

eval' regs ip instructions
  | ip > snd (bounds instructions)    = []
  | otherwise  = --trace (show (regs, ip, instructions ! ip)) $ 
                 case instructions ! ip of
                   Cpy (Reg src) (Reg dest) -> eval' (regs // [(dest, regs ! src)]) (ip+1) instructions
                   Cpy (Lit val) (Reg dest) -> eval' (regs // [(dest, val)]) (ip+1) instructions
                   Inc (Reg r) -> eval' (regs // [(r, (regs ! r) + 1)]) (ip+1) instructions
                   Dec (Reg r) -> eval' (regs // [(r, (regs ! r) - 1)]) (ip+1) instructions
                   Jnz (Reg r) (Lit offset)   -> jump (regs ! r) offset
                   Jnz (Lit val) (Lit offset) -> jump val offset
                   Jnz (Lit val) (Reg r)      -> jump val (regs ! r)
                   Jnz (Reg r1) (Reg r2)      -> jump (regs ! r1) (regs ! r2)
                   Toggle (Reg r) -> let offset = regs ! r
                                         instructions' = toggle (ip + offset) instructions
                                      in eval' regs (ip+1) instructions'
                   Mul (Reg a) (Reg b) (Reg c) -> let av = regs ! a
                                                      bv = regs ! b
                                                   in eval' (regs // [(c, av * bv)]) (ip+1) instructions
                   Out (Lit l) -> l : eval' regs (ip+1) instructions
                   Out (Reg r) -> (regs ! r) : eval' regs (ip+1) instructions
                   _ -> trace ("Skipping " ++ show (instructions ! ip)) $ eval' regs (ip+1) instructions
  where jump cond offset = let offset' = if cond == 0 then 1 else offset
                            in eval' regs (ip+offset') instructions

toggle :: Int -> Code -> Code
toggle ip instructions | bounds instructions `inRange` ip       = instructions // [(ip, instr)]
                       | otherwise                              = instructions
  where instr = toggleInstr (instructions ! ip)

toggleInstr :: Instruction -> Instruction
toggleInstr i = case i of
                  Inc a -> Dec a
                  Dec a -> Inc a
                  Toggle a -> Inc a
                  Jnz a1 a2 -> Cpy a1 a2
                  Cpy a1 a2 -> Jnz a1 a2

validOutput (0:1:xs) = validOutput xs
validOutput _ = False

foo = toArray $ optimize $ map parse input

validate a = do putStrLn $ "Trying " ++ show a
                let xs = eval a foo
                if not (validOutput xs)
                  then validate (a+1)
                  else return ()

main = validate 0
