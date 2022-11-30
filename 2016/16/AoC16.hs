module Main where

import qualified Data.ByteString as B
--import qualified Data.ByteString.Char8 as C8

input = "01111001100111011"
input' = [ c == '1' | c <- input ]

enbiggen s = s ++ [False] ++ map flip (reverse s)
  where flip False = True
        flip True = False

enbiggenTo n s | length s >= n  = take n s
               | otherwise      = enbiggenTo n (enbiggen s)

checksum s = checksum' s []

checksum' [] c | even (length c)        = checksum (reverse c)
               | otherwise              = reverse c
checksum' (x:y:zs) c = checksum' zs (cs x y : c)
  where cs x y | x == y         = True
               | otherwise      = False

printBools xs = putStrLn [ if x then '1' else '0' | x <- xs ]

{-
input2 = B.pack $ [ read [c] | c <- input ]

enbiggen2 s = s `B.append` (0 `B.cons` (B.map flip (B.reverse s)))
  where flip 0 = 1
        flip 1 = 0

enbiggenTo2 n s | B.length s >= n       = B.take n s
                | otherwise             = enbiggenTo2 n (enbiggen2 s)

checksum2 s = foo s B.empty
  where foo s c | B.null s = if even (B.length c) then checksum2 c else c
                | otherwise = let x = s `B.index` 0
                                  y = s `B.index` 1
                                  zs = B.drop 2 s
                               in foo zs (c `B.snoc` cs x y)
        cs x y | x == y         = 1
               | otherwise      = 0

printBS s = putStrLn [ if x == 0 then '0' else '1' | x <- B.unpack s ]
-}


--main = print $ checksum (enbiggenTo 35651584 input)
main = printBools $ checksum (enbiggenTo 35651584 input')
--main = printBS $ checksum2 (enbiggenTo2 35651584 input2)
