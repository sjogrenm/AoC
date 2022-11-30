module Foo where

import Data.List
import Data.Char
import Data.Graph
import Debug.Trace

input = do strs <- readFile "input.txt"
           return . map words . lines $ strs

testdata = [ "value 5 goes to bot 2"
           , "bot 2 gives low to bot 1 and high to bot 0"
           , "value 3 goes to bot 1"
           , "bot 1 gives low to output 1 and high to bot 0"
           , "bot 0 gives low to output 2 and high to output 0"
           , "value 2 goes to bot 2" ]

newtype Bot = Bot Int
        deriving (Show, Eq, Ord)
newtype Output = Output Int
        deriving (Show, Eq, Ord)
data Dest = ToBot Bot | ToOutput Output
        deriving (Show, Eq)
data Instruction = ValueTo Int Bot | Give Bot Dest Dest
        deriving Show

parse ["value",v,"goes","to","bot",b] = ValueTo (read v) (Bot $ read b)
parse ["bot",b_from,"gives","low","to",dest_low,b_low,"and","high","to",dest_high,b_high]
  = Give (Bot $ read b_from) low high
  where low = if dest_low == "output" then ToOutput (Output $ read b_low) else ToBot (Bot $ read b_low)
        high = if dest_high == "output" then ToOutput (Output $ read b_high) else ToBot (Bot $ read b_high)

parsedInput = do i <- input
                 return $ map parse i

parsedValueTo = do pi <- parsedInput
                   return [ i | i@(ValueTo _ _) <- pi ]


data State = State [(Bot,Int)] [(Output,Int)]
        deriving Show

inputs [] = []
inputs (i@(ValueTo v b):is) = i : inputs is
inputs (_:is) = inputs is

outputs [] = []

data GNode = GVal Int | GBot Bot | GOutput Output
        deriving (Eq,Ord,Show)

mkGraph [] = []
mkGraph (i@(ValueTo v b):is) = (i, GVal v, [GBot b]) : mkGraph is
mkGraph (i@(Give b d1 d2):is) = (i, GBot b, [edge d1, edge d2]) : mkGraph is
  where edge (ToBot b) = GBot b
        edge (ToOutput o) = GOutput o

mkSCC pi = [ i | AcyclicSCC i <- stronglyConnComp (mkGraph pi) ]

ioGraph = do pi <- parsedInput
             let g = mkGraph pi
             let scc = reverse [ i | AcyclicSCC i <- stronglyConnComp g ]
             return scc

instance Show a => Show (SCC a) where
    show (AcyclicSCC v) = "AcylicSCC " ++ show v
--    show (CyclicSCC vs) = "CyclicSCC " ++ show vs

apply (ValueTo v b)  (State bs os) = State ((b,v):bs) os
apply (Give b d1 d2) (State bs os) = State bs'' os'
  where ([(_,v1),(_,v2)],bs') = partition (\(bot,_) -> bot == b) bs
        low = min v1 v2
        high = max v1 v2
        (os', bs'') = case (d1, d2) of
                       (ToOutput o1, ToOutput o2) -> ((o1,low):(o2,high):os, bs')
                       (ToOutput o1, ToBot b2)    -> ((o1,low):os, (b2,high):bs')
                       (ToBot b1,    ToOutput o2) -> ((o2,high):os, (b1,low):bs')
                       (ToBot b1,    ToBot b2)    -> (os, (b1,low):(b2,high):bs')

run1 (i:is) s = case xs of
                  [(Bot b1,_), (Bot b2,_)] -> if b1 == b2 then b1 else run1 is s'
                  _ -> run1 is s'
  where s'@(State bs os) = apply i s
        (xs,ys) = partition (\(b,v) -> v `elem` [61,17]) bs


result1 = do scc <- ioGraph
             return $ run1 scc (State [] [])
--             return (bs,os)
--             return [ b | (Bot b, v) <- bs, v `elem` [61, 17] ]

run2 [] (State bs os) = os
run2 (i:is) s = run2 is (apply i s)

result2 = do scc <- ioGraph
             let os = run2 scc (State [] [])
             return $ product [ v | (Output o,v) <- os, o `elem` [0,1,2] ]

{-

ValueTo v b --> b -give 

-}
