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
        {-
        = AddR Reg Reg Reg
        | AddI Reg Val Reg
        | MulR Reg Reg Reg
        | MulI Reg Val Reg
        | BanR Reg Reg Reg
        | BanI Reg Val Reg
        | BorR Reg Reg Reg
        | BorI Reg Val Reg
        | SetR Reg Reg
        | SetI Val Reg
        | GtIR Val Reg Reg
        | GtRI Reg Val Reg
        | GtRR Reg Reg Reg
        | EqIR Val Reg Reg
        | EqRI Reg Val Reg
        | EqRR Reg Reg Reg
        -}
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
readProgram = parseInput f "16b.txt"
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

readR regs r = regs !! r
writeR regs r v = rs1 ++ v:rs2
  where (rs1,_:rs2) = splitAt r regs


sol1 ss = length [ r | (_,r) <- map findInstr ss, length r >= 3 ]

findInstr (before, op, args, after) = (op, [ i | i <- allInstr, eval i args before == after ])

debugInstr (before, _, args, after) = (before, args, after, [ (i, eval i args before) | i <- allInstr ])


foo ss = process [] $ nub $ sort [ (op,is) | (op,is) <- map findInstr ss ]
  where process result [] = result
        process result xs = process ((op,i):result) [ (o,zs\\[i]) | (o,zs) <- as++cs ]
          where (as, (op,[i]):cs) = span (\(op,is) -> length is > 1) xs


exec _ [] regs = regs
exec itab ((i,args):ops) regs = exec itab ops (eval i args regs)


sol2 itab code = exec itab code [0,0,0,0]


main = do s <- readSample
          print (sol1 s)
          let itab = foo s
          p <- readProgram
          let code = [ (fromJust (lookup op itab), args) | (op, args) <- p ]
          print (sol2 itab code)
