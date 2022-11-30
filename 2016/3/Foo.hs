module Foo where

import Data.List (sort)

input :: IO [[Int]]
input = do strs <- readFile "input.txt"
           let ls = lines strs
               wss = map words ls
           return $ map (map read) wss

possibleTriangle xs = x + y > z
  where [x,y,z] = sort xs

result1 = do i <- input
             return $ length (filter possibleTriangle i)

regroup ([a,b,c]:[d,e,f]:[g,h,i]:xs) = [a,d,g]:[b,e,h]:[c,f,i]:regroup xs
regroup _ = []


result2 = do i <- input
             let i' = regroup i
             return $ length (filter possibleTriangle i')

