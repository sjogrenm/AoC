import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
import Data.Array
import Debug.Trace (trace, traceShow)
import qualified Data.Vector as V
--import Data.Vector ((!))
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies
import Data.Graph as G
import Data.Bits
import Text.Parsec
import Text.Parsec.Char

import Util


newtype Reg = R Int
        deriving (Eq,Show)
type Val = Int
data Instruction
        = AddR
        | AddI
        | MulR
        | MulI
        | BanR
        | BanI
        | BorR
        | BorI
        | SetR
        | SetI
        | GtIR
        | GtRI
        | GtRR
        | EqIR
        | EqRI
        | EqRR
        | Nop
        deriving (Eq,Show,Ord)

allInstr = [AddR, AddI, MulR, MulI, BanR, BanI, BorR, BorI, SetR, SetI, GtIR, GtRI, GtRR, EqIR, EqRI, EqRR]

readSample:: IO [([Int], Int, (Int,Int,Int), [Int])]
readSample= do f <- readFile "16a.txt"
               let ls = lines f
                   gs = groupRuns ls
                   gs' = [ (read b', x, (y,z,w), read a')
                         | [b,i,a] <- gs,
                           let b' = drop (length "Before: ") b,
                           let [x,y,z,w] = map read (words i),
                           let a' = drop (length "After: ") a
                         ]
               return gs'

readProgram :: IO [(Int, (Int,Int,Int))]
readProgram = parseInput f "19.txt"
  where f xs = let [a,b,c,d] = map read (words xs)
                in (a, (b,c,d))

groupRuns (a:b:c:"":xs) = [a,b,c] : groupRuns xs
groupRuns _ = []

eval :: Instruction -> (Int,Int,Int) -> [Int] -> [Int]
eval AddR (ra, rb, rc) regs = writeR regs rc (readR regs ra + readR regs rb)
eval AddI (ra, vb, rc) regs = writeR regs rc (readR regs ra + vb)
eval MulR (ra, rb, rc) regs = writeR regs rc (readR regs ra * readR regs rb)
eval MulI (ra, vb, rc) regs = writeR regs rc (readR regs ra * vb)
eval BanR (ra, rb, rc) regs = writeR regs rc (readR regs ra .&. readR regs rb)
eval BanI (ra, vb, rc) regs = writeR regs rc (readR regs ra .&. vb)
eval BorR (ra, rb, rc) regs = writeR regs rc (readR regs ra .|. readR regs rb)
eval BorI (ra, vb, rc) regs = writeR regs rc (readR regs ra .|. vb)
eval SetR (ra, _,  rc) regs = writeR regs rc (readR regs ra)
eval SetI (va, _,  rc) regs = writeR regs rc va
eval GtIR (va, rb, rc) regs = writeR regs rc (if va > readR regs rb then 1 else 0)
eval GtRI (ra, vb, rc) regs = writeR regs rc (if readR regs ra > vb then 1 else 0)
eval GtRR (ra, rb, rc) regs = writeR regs rc (if readR regs ra > readR regs rb then 1 else 0)
eval EqIR (va, rb, rc) regs = writeR regs rc (if va == readR regs rb then 1 else 0)
eval EqRI (ra, vb, rc) regs = writeR regs rc (if readR regs ra == vb then 1 else 0)
eval EqRR (ra, rb, rc) regs = writeR regs rc (if readR regs ra == readR regs rb then 1 else 0)
eval Nop _ regs = regs

readR regs r = regs !! r
writeR regs r v = rs1 ++ v:rs2
  where (rs1,_:rs2) = splitAt r regs



findInstr (before, op, args, after) = (op, [ i | i <- allInstr, eval i args before == after ])

debugInstr (before, _, args, after) = (before, args, after, [ (i, eval i args before) | i <- allInstr ])


foo ss = process [] $ nub $ sort [ (op,is) | (op,is) <- map findInstr ss ]
  where process result [] = result
        process result xs = process ((op,i):result) [ (o,zs\\[i]) | (o,zs) <- as++cs ]
          where (as, (op,[i]):cs) = span (\(op,is) -> length is > 1) xs



ipR = 4

exec ipR program regs
  | ip `inBounds` program       = let (instr,args) = {- traceShow (ip,regs) $ -} program ! ip
                                      regs' = eval instr args regs
                                      regs'' = writeR regs' ipR $ readR regs' ipR + 1
                                   in {- (ip,regs) : -} exec ipR program regs''
  | otherwise = regs
  where ip = readR regs ipR


--sol2 itab code = exec itab code [0,0,0,0]



sol1 reg0 = exec ipR program [reg0,0,0,0,0,0]

--sol2 = exec ipR program [1,0,0,0,0,0]
--sol2 = exec ipR program [0,10551386,10551386,10551386,4,1]
                        --[0,10551386,10551386,5275693,4,2]
                        --[2,10551386,10551386,10551387,4,3]

main = do
        print $ sol1 5970144
   --     print sol1
   --     print sol2


parseLine :: String -> (Instruction, (Int,Int,Int))
parseLine xs = (i, (read b, read c, read d))
  where [a,b,c,d] = words xs
        i = case a of
              "addr" -> AddR
              "addi" -> AddI
              "mulr" -> MulR
              "muli" -> MulI
              "banr" -> BanR
              "bani" -> BanI
              "borr" -> BorR
              "bori" -> BorI
              "setr" -> SetR
              "seti" -> SetI
              "gtir" -> GtIR
              "gtri" -> GtRI
              "gtrr" -> GtRR
              "eqir" -> EqIR
              "eqri" -> EqRI
              "eqrr" -> EqRR
              "nop"  -> Nop
              
--[2,10551386,10551386,10551387,4,3]

program = mkArray $ map parseLine
  [ "seti 123 0 1"
  , "bani 1 456 1"
  , "eqri 1 72 1"
  , "addr 1 4 4"
  , "seti 0 0 4"
  , "seti 0 3 1"        -- 5
  , "bori 1 65536 5"
  , "seti 8586263 3 1"
  , "bani 5 255 2"
  , "addr 1 2 1"
  , "bani 1 16777215 1" -- 10
  , "muli 1 65899 1"
  , "bani 1 16777215 1"
  , "gtir 256 5 2"
  , "addr 2 4 4"
  , "addi 4 1 4"        -- 15
  , "seti 27 8 4"
  , "seti 0 1 2"
  , "addi 2 1 3"
  , "muli 3 256 3"
  , "gtrr 3 5 3"        -- 20
  , "addr 3 4 4"
  , "addi 4 1 4"
  , "seti 25 8 4"
  , "addi 2 1 2"
  , "seti 17 7 4"       -- 25
  , "setr 2 0 5"
  , "seti 7 8 4"
  , "eqrr 1 0 2"        -- 28
  , "addr 2 4 4"
  , "seti 5 4 4"        -- 30
  ]

{-
 0: 123           -> r1
 1: r1 & 456      -> r1
 2: r1 == 72      -> r1
 3: if r1 goto 5
 4: goto 0
 5: 0             -> r1
 6: r1 | 65536    -> r5
 7: 8586263       -> r1
 8: r5 & 255      -> r2
 9: r1 + r2       -> r1
10: r1 & 16777215 -> r1
11: r1 * 65899    -> r1
12: r1 & 16777215 -> r1
13: 256 > r5      -> r2
14: if r2 goto 16
15: goto 17
16: goto 28
17: 0             -> r2
18: r2 + 1        -> r3
19: r3 * 256      -> r3
20: r3 > r5       -> r3
21: if r3 goto 23
22: goto 24
23: goto 26
24: r2 + 1        -> r2
25: goto 18
26: r2            -> r5
27: goto 8
28: r1 == r0      -> r2
29: if r2 goto 31
30: goto 6
-}
{-

r1 := 0
do
    r5 := r1 | 65536
    r1 := 8586263
    do
        r2 := r5 & 255
        r1 := r1 + r2
        r1 := r1 & 16777215
        r1 := r1 * 65899
        r1 := r1 & 16777215
        if 256 > r5
            break
        r2 := 0
        do
            r3 := r2 + 1
            r3 := r3 * 256
            if r3 > r5
                r5 := r2
                break
            else
                r2 := r2 + 1
        while true
    while true
while r1 != r0
-}



reg1foo r1 = (((8586263 + r2) .&. 16777215) * 65899) .&. 16777215
  where r5 = r1 .|. 65536
        r2 = r5 .&. 255


reg1foo' r5 = (((8586263 + r2) .&. 16777215) * 65899) .&. 16777215
  where r2 = r5 .&. 255

