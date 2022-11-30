module Foo where

import Data.List

input = do strs <- readFile "input.txt"
           return $ lines strs

testdata = ["rect 3x2", "rotate column x=1 by 1", "rotate row y=0 by 4", "rotate column x=1 by 1"]

data Instruction = Rect Int Int | RotateRow Int Int | RotateCol Int Int
        deriving Show

parse xs = case words xs of
  ["rect", rect] -> let (a, 'x':b) = span (/='x') rect
                     in Rect (read a) (read b)
  ["rotate", "row",    'y':'=':y, "by", count] -> RotateRow (read y) (read count)
  ["rotate", "column", 'x':'=':x, "by", count] -> RotateCol (read x) (read count)

newtype Display = D [[Bool]]

instance Show Display where
    show (D []) = ""
    show (D (l:ls)) = showLine l ++ "\n" ++ show (D ls)
      where showLine = map (\c -> if c then '#' else '.')

apply (D bitmap) (Rect w h)
  = D [ if i < h then replicate w True ++ drop w line else line
      | (line, i) <- zip bitmap [0..] ]
apply (D bitmap) (RotateRow row count)
  = D [ if i == row then let (xs,ys) = splitAt (length line - count) line in ys ++ xs else line
      | (line, i) <- zip bitmap [0..] ]
apply (D bitmap) (RotateCol col count)
  = let temp = D (transpose bitmap)
        D temp' = apply temp (RotateRow col count)
     in D (transpose temp')

initialState = D $ replicate 6 (replicate 50 False)

countTrue (D bitmap) = length $ concatMap (filter id) bitmap

result1 = do i <- input
             let is = map parse i
             return $ countTrue $ foldl apply initialState is
