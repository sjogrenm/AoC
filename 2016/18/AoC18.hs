module Main where

input = ".^^^.^.^^^.^.......^^.^^^^.^^^^..^^^^^.^.^^^..^^.^.^^..^.^..^^...^.^^.^^^...^^.^.^^^..^^^^.....^...."

mkRoom prev = prev : mkRoom line
  where line = [ t [l,c,r] | (l,c,r) <- zip3 ('.':prev) prev (drop 1 prev ++ ['.']) ]
        t "^^." = '^'
        t "..^" = '^'
        t ".^^" = '^'
        t "^.." = '^'
        t _ = '.'

room n f = take n (mkRoom f)

printRoom xs = mapM_ putStrLn xs

countSafe r = length $ filter (=='.') $ concat r

main = print $ countSafe $ room 400000 input
