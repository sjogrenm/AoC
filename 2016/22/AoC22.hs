module Main where

import Data.Char
import Data.List

type Node = (Int,Int)

data Usage = Usage Node Int Int Int
        deriving (Show)

input :: IO [Usage]
input = do strs <- readFile "input.txt"
           return . map (parse . words) . drop 2 . lines $ strs

parse [node,szT,usedT,availT,_] = Usage (read x,read y) sz used avail
  where (x,'-':'y':y) = span isDigit (drop (length "/dev/grid/node-x") node)
        sz = read (init szT)
        used = read (init usedT)
        avail = read (init availT)

canMove :: Usage -> Usage -> Bool
canMove (Usage pa _ ua _) (Usage pb _ _ ab) = pa /= pb && ua /= 0 && ua <= ab

viable :: [Usage] -> [(Usage, Usage)]
viable xs = [ (a,b) | a <- xs, b <- xs, canMove a b ]

getRow i xs = [ x | x@(Usage (_,k) _ _ _) <- xs, i == k ]

findEmpty xs = [ x | x@(Usage _ _ 0 _) <- xs ]


arrange :: [Usage] -> [[Usage]]
arrange xs = [ getRow i xs | i <- [0..24] ]

visualize xss = unlines $ [ map v xs | xs <- xss ]
  where v (Usage _ s u a) | s > 100     = '#'
                          | u == 0      = '_'
                          | otherwise   = '.'


main = print ()

{-

to x = 31
x to y = 33


..G_
....
->
.G_.
....
tar 5 

37 * 5

.....x...............................Gy
.......................................
.......................................
.......................................
.......................................
.......................................
.......................................
.......................................
.......................................
.......................................
.......................................
.......................................
.......................................
.......................................
.......................................
.......................................
.......................................
.......................................
.......................................
......#################################
.......................................
.......................................
.......................................
............._.........................
.......................................


-}
