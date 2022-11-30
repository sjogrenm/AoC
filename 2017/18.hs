import Control.Monad.ST
import Data.Char
import Data.List
import Data.List.Split
import Data.Array
import Debug.Trace

parseInput = do i <- readFile "18input.txt"
                return $ arrayFromList $ parseStr i

arrayFromList xs = listArray (0, length xs - 1) xs
i `withinBounds` a = let (lb,ub) = bounds a
                      in i >= lb && i < ub

data Val = C Int | R Char
        deriving Show

data Instr = Snd Val | Set Char Val | Add Char Val | Mul Char Val | Mod Char Val | Rcv Char | Jgz Val Val
        deriving Show

pv [x] | isAlpha x      = R x
pv y                    = C (read y)

parseStr i = [ parse (words x) | x <- lines i ]

parse ["snd",v]     = Snd (pv v)
parse ["set",[r],v] = Set r (pv v)
parse ["add",[r],v] = Add r (pv v)
parse ["mul",[r],v] = Mul r (pv v)
parse ["mod",[r],v] = Mod r (pv v)
parse ["rcv",[r]]   = Rcv r
parse ["jgz",v1,v2] = Jgz (pv v1) (pv v2)

eval1 instructions = eval' (listArray ('a', 'p') (repeat 0), -1) 0
  where
  eval' :: (Array Char Int, Int) -> Int -> (Array Char Int, Int)
  eval' (regs, currSnd) ip
    | ip `withinBounds` instructions = case instructions ! ip of
        Snd v   -> --trace ("snd "++show (val regs v)) $
                   eval' (regs, val regs v) (ip+1)
        Set r v -> eval' (regs // [(r, val regs v)], currSnd) (ip+1)
        Add r v -> eval' (regs // [(r, val regs (R r) + val regs v)], currSnd) (ip+1)
        Mul r v -> eval' (regs // [(r, val regs (R r) * val regs v)], currSnd) (ip+1)
        Mod r v -> eval' (regs // [(r, val regs (R r) `mod` val regs v)], currSnd) (ip+1)
        Rcv r   -> --trace ("rcv "++show (val regs (R r), currSnd)) $
                   case val regs (R r) of
                     0 -> eval' (regs, currSnd) (ip+1)
                     n -> (regs, currSnd)
        Jgz v1 v2 | val regs v1 > 0     -> eval' (regs, currSnd) (ip + val regs v2)
                  | otherwise           -> eval' (regs, currSnd) (ip+1)
  eval' regAndSnd _ = regAndSnd

val regs (C i) = i
val regs (R x) = regs ! x


sampleStr = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
sample = arrayFromList (parseStr sampleStr)


--                     id  regs             ip  incoming    tot sent
data ProgramState = PS Int (Array Char Int) Int (Queue Int) Int
        deriving Show


x `sendTo` (PS id regs ip q ts) = PS id regs ip (x `queuePut` q) ts

receive (PS id regs ip q ts) | queueEmpty q      = Nothing
                             | otherwise         = let (x,q') = queueGet q
                                                    in Just (x, PS id regs ip q' ts)

setReg r v (PS id regs ip q ts) = PS id (regs // [(r,v)]) ip q ts

incIP (PS id regs ip q ts) inc = PS id regs (ip+inc) q ts

data Queue a = MkQueue [a] [a]
        deriving Show

mkQueue :: Queue a
mkQueue = MkQueue [] []

queuePut :: a -> Queue a -> Queue a
queuePut item (MkQueue ins outs) = MkQueue (item:ins) outs

queuePutList :: [a] -> Queue a -> Queue a
queuePutList xs q = foldl' (flip queuePut) q xs

queueGet :: Queue a -> (a, Queue a)
queueGet (MkQueue ins (item:rest)) = (item, MkQueue ins rest)
queueGet (MkQueue ins []) = queueGet (MkQueue [] (reverse ins))

queueEmpty :: Queue a -> Bool
queueEmpty (MkQueue ins outs) = null ins && null outs

eval2 instructions = eval' p0 p1 False
  where
  regs = listArray ('a', 'p') (repeat 0)
  p0 = PS 0 regs 0 mkQueue 0
  p1 = PS 1 (regs // [('p', 1)]) 0 mkQueue 0
  eval' p@(PS id regs ip q ts) op blocked
    | ip `withinBounds` instructions = case instructions ! ip of
        Snd v   -> eval' (incIP (PS id regs ip q (ts+1)) 1) (val regs v `sendTo` op) False
        Set r v -> eval' (incIP (setReg r (val regs v)                      p) 1) op blocked
        Add r v -> eval' (incIP (setReg r (val regs (R r)   +   val regs v) p) 1) op blocked
        Mul r v -> eval' (incIP (setReg r (val regs (R r)   *   val regs v) p) 1) op blocked
        Mod r v -> eval' (incIP (setReg r (val regs (R r) `mod` val regs v) p) 1) op blocked
        Rcv r   -> case (receive p, blocked) of
                     (Just (val, p'), _) -> eval' (incIP (setReg r val p') 1) op blocked
                     (Nothing, False)    -> eval' op p True
                     (Nothing, True)     -> (p,op) -- both are waiting, terminate
        Jgz v1 v2 | val regs v1 > 0     -> eval' (incIP p (val regs v2)) op blocked
                  | otherwise           -> eval' (incIP p 1)             op blocked
  eval' p op True = (p, op)
  eval' p op False = eval' op p True


sol2 = do i <- parseInput
          return (eval2 i)

sample2 = arrayFromList [Snd (C 1), Snd (C 2), Snd (R 'p'), Rcv 'a', Rcv 'b', Rcv 'c', Rcv 'd']
