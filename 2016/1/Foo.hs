module Foo where

import Data.List.Split

--north = 0
--east = 1
--south = 2
--west = 3

--right angle = 1
--left angle = -1

turn dir angle = (dir + angle) `mod` 4

x_dir x = case x of 0 -> 0 ; 1 -> 1 ; 2 -> 0 ; 3 -> -1
y_dir y = case y of 0 -> 1 ; 1 -> 0 ; 2 -> -1 ; 3 -> 0


move (dir, x, y) steps = (dir, x + (x_dir dir) * steps, y + (y_dir dir) * steps)

app (dir,x,y) (angle,steps) = move (turn dir angle, x, y) steps


parse s = [ (angle x, read xs :: Int) | (x:xs) <- splitOn ", " s ]
  where angle 'R' = 1
        angle 'L' = -1

input = parse "L3, R2, L5, R1, L1, L2, L2, R1, R5, R1, L1, L2, R2, R4, L4, L3, L3, R5, L1, R3, L5, L2, R4, L5, R4, R2, L2, L1, R1, L3, L3, R2, R1, L4, L1, L1, R4, R5, R1, L2, L1, R188, R4, L3, R54, L4, R4, R74, R2, L4, R185, R1, R3, R5, L2, L3, R1, L1, L3, R3, R2, L3, L4, R1, L3, L5, L2, R2, L1, R2, R1, L4, R5, R4, L5, L5, L4, R5, R4, L5, L3, R4, R1, L5, L4, L3, R5, L5, L2, L4, R4, R4, R2, L1, L3, L2, R5, R4, L5, R1, R2, R5, L2, R4, R5, L2, L3, R3, L4, R3, L2, R1, R4, L5, R1, L5, L3, R4, L2, L2, L5, L5, R5, R2, L5, R1, L3, L2, L2, R3, L3, L4, R2, R3, L1, R2, L5, L3, R4, L4, R4, R3, L3, R1, L3, R5, L5, R1, R5, R3, L1"


crossings (dir, x, y) steps = [ (x + (x_dir dir) * k, y + (y_dir dir) * k) | k <- [1..steps] ]

app2 (dir,x,y,visited) (angle,steps) = (d',x',y', reverse (crossings (d',x,y) steps) ++ visited)
  where (d',x',y') = app (dir,x,y) (angle,steps)

find_dup xs = [ head ys | x <- xs, let ys = filter (==x) xs, length ys > 1 ]
