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
  | ip `inBounds` program       = let (instr,args) = traceShow (ip,regs) $ program ! ip
                                      regs' = eval instr args regs
                                      regs'' = writeR regs' ipR $ readR regs' ipR + 1
                                   in {- (ip,regs) : -} exec ipR program regs''
  | otherwise = regs
  where ip = readR regs ipR


--sol2 itab code = exec itab code [0,0,0,0]



sol1 = exec ipR program [0,0,0,0,0,0]

--sol2 = exec ipR program [1,0,0,0,0,0]
sol2 = exec ipR program [0,10551386,10551386,10551386,4,1]
                        --[0,10551386,10551386,5275693,4,2]
                        --[2,10551386,10551386,10551387,4,3]

main = do
   --     print sol1
        print sol2


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
  [ "addi 4 16 4"
  , "seti 1 3 5"
  , "seti 1 1 3"
  , "mulr 5 3 1"
  , "eqrr 1 2 1"
  , "addr 1 4 4"        -- 5
  , "addi 4 1 4"
  , "addr 5 0 0"
  , "addi 3 1 3"
  , "gtrr 3 2 1"
  , "addr 4 1 4"        -- 10
  , "seti 2 8 4"
  , "addi 5 1 5"
  , "gtrr 5 2 1"
  , "addr 1 4 4"
  , "seti 1 3 4"        -- 15
  , "mulr 4 4 4"
  , "addi 2 2 2"
  , "mulr 2 2 2"
  , "mulr 4 2 2"
  , "muli 2 11 2"       -- 20
  , "addi 1 6 1"
  , "mulr 1 4 1"
  , "addi 1 18 1"
  , "addr 2 1 2"
  , "addr 4 0 4"        -- 25
  , "seti 0 3 4"
  , "setr 4 5 1"
  , "mulr 1 4 1"
  , "addr 4 1 1"
  , "mulr 4 1 1"        -- 30
  , "muli 1 14 1"
  , "mulr 1 4 1"
  , "addr 2 1 2"
  , "seti 0 1 0"
  , "seti 0 4 4" ]      -- 35

sample = mkArray $ map parseLine
  [ "seti 5 0 1"
  , "seti 6 0 2"
  , "addi 0 1 0"
  , "addr 1 2 3"
  , "setr 1 0 0"
  , "seti 8 0 4"
  , "seti 9 0 5" ]


factors 1 = []
factors n = case [ (k,d) | k <- [2..n`div`2], let (d,m) = n `divMod` k, m == 0 ] of
              []          -> [n]
              ((k',d'):_) -> k' : factors d'
