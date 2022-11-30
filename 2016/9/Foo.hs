module Foo where

import Data.List
import Data.Char

input = do strs <- readFile "input.txt"
           return strs

testdata = ["ADVENT", "A(1x5)BC", "(3x3)XYZ", "A(2x2)BCD(2x2)EFG", "(6x1)(1x3)A", "X(8x2)(3x3)ABCY"]

decompress1 :: String -> String
decompress1 [] = ""
decompress1 ('(':xs) = concat (replicate (read b) cc) ++ decompress1 dd
  where (a,'x':ys) = span isDigit xs
        (b,')':zs) = span isDigit ys
        (cc,dd) = splitAt (read a) zs
decompress1 (x:xs) = x : decompress1 xs

result1 = do i <- input
             let d = decompress1 i
             return $ sum (map length $ lines d)

decompress2 :: String -> String
decompress2 [] = ""
decompress2 ('(':xs) = decompress2 (concat (replicate (read b) cc) ++ dd)
  where (a,'x':ys) = span isDigit xs
        (b,')':zs) = span isDigit ys
        (cc,dd) = splitAt (read a) zs
decompress2 (x:xs) = x : decompress2 xs

result2 = do i <- input
             let d = decompress2len i
             return $ d

decompress2len :: String -> Int
decompress2len [] = 0
decompress2len ('(':xs) = read b * cc_len + decompress2len dd
  where (a,'x':ys) = span isDigit xs
        (b,')':zs) = span isDigit ys
        (cc,dd) = splitAt (read a) zs
        cc_len = decompress2len cc
decompress2len (x:xs) | isSpace x = decompress2len xs
decompress2len (x:xs)             = decompress2len xs + 1


{-

(8x2)(3x3)ABC
->
(3x3)ABC(3x3)ABC
->
ABCABCABCABCABCABC


-}
