module Main where

import Data.List

data Instruction = SwapPos Int Int
                 | SwapChar Char Char
                 | Rotate (Either Int Int)
                 | RotateChar Char
                 | ReverseRange Int Int
                 | Move Int Int
        deriving (Eq, Show)

parse ["swap","position",i,"with","position",j] = SwapPos (read i) (read j)
parse ["swap","letter",[c],"with","letter",[d]] = SwapChar c d
parse ["rotate","left",n,_] = Rotate (Left (read n))
parse ["rotate","right",n,_] = Rotate (Right (read n))
parse ["reverse","positions",i,"through",j] = ReverseRange (read i) (read j)
parse ["rotate","based","on","position","of","letter",[c]] = RotateChar c
parse ["move","position",i,"to","position",j] = Move (read i) (read j)


testinstr = [ SwapPos 4 0, SwapChar 'd' 'b', ReverseRange 0 4, Rotate (Left 1)
            , Move 1 4, Move 3 0, RotateChar 'b', RotateChar 'd' ]

eval :: Instruction -> String -> String
eval (SwapPos i j) s = xs ++ [d] ++ zs ++ [c] ++ ws
  where i' = min i j
        j' = max i j
        (xs,c:ys) = splitAt i' s
        (zs,d:ws) = splitAt (j' - i' - 1) ys
        -- abcdef       ab, c:def
        --   ^ ^                  d,e:f
eval (SwapChar c d) s = eval (SwapPos i j) s
  where Just i = elemIndex c s
        Just j = elemIndex d s

eval (Rotate (Left i)) s = drop i' s ++ take i' s
  where i' = i `mod` length s
eval (Rotate (Right i)) s = eval (Rotate (Left (length s - i))) s

eval (RotateChar c) s = eval (Rotate (Right (1 + i + e))) s
  where Just i = elemIndex c s
        e = if i >= 4 then 1 else 0

eval (ReverseRange i j) s = xs ++ reverse zs ++ ws
  where i' = min i j
        j' = max i j
        (xs,ys) = splitAt i' s
        (zs,ws) = splitAt (j' - i' + 1) ys
        -- abcdef       ab,cdef
        --                      cd,ef
        
eval (Move i j) s = take j zs ++ [c] ++ drop j zs
  where (xs,c:ys) = splitAt i s
        zs = xs ++ ys

feval = flip eval

evalAll s xs = foldl feval s xs

input :: IO [Instruction]
input = do strs <- readFile "input.txt"
           return . map (parse . words) . lines $ strs

uneval (SwapPos i j) s = eval (SwapPos i j) s

uneval (SwapChar c d) s = eval (SwapChar c d) s

uneval (Rotate (Left i)) s = eval (Rotate (Right i)) s
uneval (Rotate (Right i)) s = eval (Rotate (Left i)) s

uneval (RotateChar c) s = eval (Rotate (Left j)) s
  where Just i = elemIndex c s
        Just j = lookup i [(1,1),(3,2),(5,3),(7,4),(2,6),(4,7),(6,8),(0,9)]
        -- abcdefghi
        --         ^:8
        -- 0 -> 0+(1+0) = 1
        -- 1 -> 1+(1+1) = 3
        -- 2 -> 2+(1+2) = 5
        -- 3 -> 3+(1+3) = 7
        -- 4 -> 4+(2+4) = 10 = 2
        -- 5 -> 5+(2+5) = 12 = 4
        -- 6 -> 6+(2+6) = 14 = 6
        -- 7 -> 7+(2+7) = 16 = 0

uneval (ReverseRange i j) s = eval (ReverseRange i j) s

uneval (Move i j) s = eval (Move j i) s

unevalAll s xs = foldl (flip uneval) s (reverse xs)


main = print ()
