module Main where

import Control.Monad.ST
import Data.List
import Data.Char
import Data.Graph
import Debug.Trace
import Data.Array
import Data.Array.IO

input =
 [ "cpy a b"
 , "dec b"
 , "cpy a d"
 , "cpy 0 a"
 , "cpy b c"
 , "inc a"
 , "dec c"
 , "jnz c -2"
 , "dec d"
 , "jnz d -5"
 , "dec b"
 , "cpy b c"
 , "cpy c d"
 , "dec d"
 , "inc c"
 , "jnz d -2"
 , "tgl c"
 , "cpy -16 c"
 , "jnz 1 c"
 , "cpy 90 c"
 , "jnz 81 d"
 , "inc a"
 , "inc d"
 , "jnz d -2"
 , "inc c"
 , "jnz c -5"
 ]

testinput =
 [ "cpy 2 a"
 , "tgl a"
 , "tgl a"
 , "tgl a"
 , "cpy 1 a"
 , "dec a"
 , "dec a"
 ]

data Arg = Reg Char | Lit Int
        deriving (Eq,Show)

data Instruction = Cpy Arg Arg
                 | Jnz Arg Arg
                 | Inc Arg
                 | Dec Arg
                 | Toggle Arg
                 | Mul Arg Arg Arg
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

toArray xs = listArray (0,length xs - 1) xs

testdata = toArray (map parse testinput)
mydata = toArray (map parse input)

nop = Jnz (Lit 0) (Lit 0)

optimize :: [Instruction] -> [Instruction]
optimize (Cpy (Lit 0) a1 : Cpy b1 c1 : Inc a2 : Dec c2 : Jnz c3 (Lit (-2)) : Dec d1 : Jnz d2 (Lit (-5)) : is)
  | and [a1 == a2, c1 == c2, c2 == c3, d1 == d2 ]       = Mul b1 d1 a1 : replicate 6 nop ++ optimize is
optimize (i:is) = i : optimize is
optimize [] = []

eval a instructions = eval' (listArray ('a','d') [a,0,0,0]) 0 instructions

eval' regs ip instructions
  | ip > snd (bounds instructions)    = regs
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


eval2 :: [String] -> [Int] -> IO (Array Char Int)
eval2 input regVals = 
  do instructions <- newListArray (0, length input - 1) (map parse input)
     regs <- newListArray ('a', 'd') regVals
     eval2' regs 0 instructions
     freeze regs

eval2' :: IOUArray Char Int -> Int -> IOArray Int Instruction -> IO ()
eval2' regs ip instructions
  = do bounds <- getBounds instructions
       if ip > snd bounds then
         return ()
       else do instr <- readArray instructions ip
               case instr of
                   Cpy (Reg src) (Reg dest) -> do val <- readArray regs src
                                                  writeArray regs dest val
                                                  eval2' regs (ip+1) instructions
                   Cpy (Lit val) (Reg dest) -> do writeArray regs dest val
                                                  eval2' regs (ip+1) instructions
                   Inc (Reg r)              -> do val <- readArray regs r
                                                  writeArray regs r (val + 1)
                                                  eval2' regs (ip+1) instructions
                   Dec (Reg r)              -> do val <- readArray regs r
                                                  writeArray regs r (val - 1)
                                                  eval2' regs (ip+1) instructions
                   Jnz (Reg r) (Lit offset)   -> do val <- readArray regs r
                                                    jump val offset
                   Jnz (Lit val) (Lit offset) -> jump val offset
                   Jnz (Lit val) (Reg r)      -> do offset <- readArray regs r
                                                    jump val offset
                   Jnz (Reg r1) (Reg r2)      -> do val <- readArray regs r1
                                                    offset <- readArray regs r2
                                                    jump val offset
                   Toggle (Reg r) -> do offset <- readArray regs r
                                        toggle2 (ip + offset) instructions
                                        eval2' regs (ip+1) instructions
                   Mul (Reg a) (Reg b) (Reg c) -> do av <- readArray regs a
                                                     bv <- readArray regs b
                                                     writeArray regs c (av*bv)
                                                     eval2' regs (ip+1) instructions
                   _ -> --trace ("Skipping " ++ show (instructions ! ip)) $
                        eval2' regs (ip+1) instructions
  where jump cond offset = let offset' = if cond == 0 then 1 else offset
                            in eval2' regs (ip+offset') instructions
        toggle2 :: Int -> IOArray Int Instruction -> IO ()
        toggle2 ip instructions = do bounds <- getBounds instructions
                                     if bounds `inRange` ip
                                       then do instr <- readArray instructions ip
                                               writeArray instructions ip (toggleInstr instr)
                                       else return ()



main = print (eval' (listArray ('a','d') [12,0,0,0]) 0 mydata)
