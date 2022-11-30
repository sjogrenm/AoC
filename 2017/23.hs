import Debug.Trace
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Array
import qualified Data.Set as S
import qualified Data.Map.Strict as M

parseInput = do i <- readFile "23input.txt"
                return $ arrayFromList $ parseStr i

arrayFromList xs = listArray (0, length xs - 1) xs
i `withinBounds` a = let (lb,ub) = bounds a
                      in i >= lb && i <= ub

data Val = C Int | R Char
        deriving Show

data Instr = Set Char Val | Add Char Val | Sub Char Val | Mul Char Val | Jnz Val Val | Nop
        deriving Show

pv [x] | isAlpha x      = R x
pv y                    = C (read y)

parseStr i = [ parse (words x) | x <- lines i ]

parse ["set",[r],v] = Set r (pv v)
parse ["add",[r],v] = Add r (pv v)
parse ["sub",[r],'-':v] = Add r (pv v)
parse ["sub",[r],v] = Sub r (pv v)
parse ["mul",[r],v] = Mul r (pv v)
parse ["jnz",v1,v2] = Jnz (pv v1) (pv v2)

eval1 instructions rs = eval' (rs, 0) 0
  where eval' :: (Array Char Int, Int) -> Int -> (Array Char Int, Int)
        eval' (regs, mulCount) ip
          | ip `withinBounds` instructions = case instructions ! ip of
            Set r v -> eval' (regs // [(r, val regs v)], mulCount) (ip+1)
            Add r v -> (if r == 'h' then trace (show regs) else nop) $ eval' (regs // [(r, val regs (R r) + val regs v)], mulCount) (ip+1)
            Sub r v -> eval' (regs // [(r, val regs (R r) - val regs v)], mulCount) (ip+1)
            Mul r v -> eval' (regs // [(r, val regs (R r) * val regs v)], mulCount+1) (ip+1)
            Jnz v1 v2 | val regs v1 /= 0    -> {-trace (show regs) $-} eval' (regs, mulCount) (ip + val regs v2)
                      | otherwise           -> eval' (regs, mulCount) (ip+1)
            Nop                             -> eval' (regs, mulCount) (ip+1)
        eval' regAndMC _ = regAndMC
        nop x = x

val regs (C i) = i
val regs (R x) = regs ! x

sol1 = do is <- parseInput
          let rs = listArray ('a', 'h') (repeat 0)
              (regs, mc) = eval1 is rs
          return (regs, mc)

sol2 = do is <- parseInput2
          let rs = listArray ('a', 'h') (repeat 0) // [('a', 1)]
              (regs, mc) = eval1 is rs
          return (regs ! 'h')

parseInput2 = do i <- readFile "23input_mod.txt"
                 return $ arrayFromList $ parseStr i

partial = "set b 65\nset c b\nmul b 100 \nsub b -100000 \nset c b \nsub c -17000 \nset f 1 \nset d 2 \nset e 2 \nset g d \nmul g e \nsub g b\n"


--main = sol2 >>= print


foo n = [ (d,e) | d <- [2..n], let (e,r) = n `divMod` d, r == 0, e > 1 ]

sol2' n i = [ k | k <- take 1001 [n,n+i..], length (foo k) > 0 ]

main = print $ length $ sol2' 106500 17
