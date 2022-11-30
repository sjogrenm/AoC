module Foo where

import Text.Regex
import Data.List (sort, sortBy)
import Data.Char (ord, chr)

re = mkRegex "^([a-z-]+)-([0-9]*)\\[([a-z]*)\\]$"

parseLine line = (name, read id :: Int, checksum)
  where Just (_,_,_,[name,id,checksum]) = matchRegexAll re line

input = do strs <- readFile "input.txt"
           let ls = lines strs
           return $ map parseLine ls

freq xs = freq' xs []
  where freq' [] xs             = xs
        freq' (a:as) []         = freq' as [(a,1)]
        freq' (a:as) ((x,n):xs)
          | a == x              = freq' as ((x,n+1):xs)
          | otherwise           = freq' as ((a,1):(x,n):xs)

mkChecksum name = [ c | (c,_) <- take 5 f' ]
  where name' = sort (filter (/='-') name)
        f = freq name'
        cmp (x,n) (y,m) = case compare m n of
                            LT -> LT
                            EQ -> compare x y
                            GT -> GT
        f' = sortBy cmp f

isValid (name, _, checksum) = checksum == mkChecksum name

validRooms is = [ (name, id) | (name, id, checksum) <- is, checksum == mkChecksum name ]

result = do is <- input
            let rooms = validRooms is
            return $ sum [ id | (name, id) <- rooms ]

decrypt id []       = []
decrypt id ('-':xs) = ' ' : decrypt id xs
decrypt id (x:xs) = rot x id : decrypt id xs

rot x n = chr (base + (ord x - base + n) `mod` 26)
  where base = ord 'a'

result2 = do is <- input
             let rooms = validRooms is
             return [ (decrypt id name, id) | (name, id) <- rooms ]

printResult2 = do r <- result2
                  sequence_ [ putStrLn (name ++ ": " ++ show id) | (name, id) <- r ]
