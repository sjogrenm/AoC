module Foo where

import Data.List (transpose, sort, sortBy)

input = do strs <- readFile "input.txt"
           return $ lines strs

testdata = [ "eedadn", "drvtee", "eandsr", "raavrd", "atevrs", "tsrnev",
             "sdttsa", "rasrtv", "nssdts", "ntnada", "svetve", "tesnvt",
             "vntsnd", "vrdear", "dvrsen", "enarar" ]

process' cmp xs = map (head . map fst . sortBy cmp . freq . sort) (transpose xs)

process1 = process' cmp
  where cmp (x,n) (y,m) = case compare m n of
                            LT -> LT
                            EQ -> compare x y
                            GT -> GT

process2 = process' cmp
  where cmp (x,n) (y,m) = case compare n m of
                            LT -> LT
                            EQ -> compare x y
                            GT -> GT

freq xs = freq' xs []
  where freq' [] xs             = xs
        freq' (a:as) []         = freq' as [(a,1)]
        freq' (a:as) ((x,n):xs)
          | a == x              = freq' as ((x,n+1):xs)
          | otherwise           = freq' as ((a,1):(x,n):xs)

result1 = do i <- input
             return $ process1 i

result2 = do i <- input
             return $ process2 i
