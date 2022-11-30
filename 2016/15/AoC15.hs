module Main where

{-
Disc #1 has 17 positions; at time=0, it is at position 15.
Disc #2 has 3 positions; at time=0, it is at position 2.
Disc #3 has 19 positions; at time=0, it is at position 4.
Disc #4 has 13 positions; at time=0, it is at position 2.
Disc #5 has 7 positions; at time=0, it is at position 2.
Disc #6 has 5 positions; at time=0, it is at position 0.
-}

sizes = [17, 3, 19, 13, 7, 5] ++ [11]
positions = [15, 2, 4, 2, 2, 0] ++ [0]

pos_at t = [ (p + t + i) `mod` s | (p,s,i) <- zip3 positions sizes [1..] ]

time_for_z = [ t | t <- [0..], all (==0) (pos_at t) ]


main = putStrLn ""
