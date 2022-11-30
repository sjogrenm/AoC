module Main where

import Data.List
import Data.Char

input :: IO [(Int,Int)]
input = do strs <- readFile "input.txt"
           return . map f . lines $ strs
  where f xs = let (ys,'-':zs) = span isDigit xs in (read ys, read zs)

testinput = [(5,8),(0,2),(4,7)]

result1 xs = result1' (sort xs) 0
result1' ((a,b):xs) l | a <= l    = result1' xs (b+1)
                      | otherwise = l

result2 xs mu = result2' (sort xs) 0
  where result2' [] l = max (mu - l + 1) 0
        result2' ((a,b):xs) l | a <= l    = rec
                              | otherwise = (a - l) + rec
          where rec = result2' xs (max l (b+1))

testinput2 = [(0,3),(7,8),(9,20),(18,19),(22,25)]

{-
0-3
7-8
9-20
18-19
22-25
-}

main = print ()
