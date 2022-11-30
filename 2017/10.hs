import Data.Bits
import Data.Char (ord, chr)

input1 = [106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118]

revPiece ls len pos
  = ss ++ fs ++ rs ++ ds
  where (as,bs) = splitAt pos ls
        (cs,ds) = splitAt len bs
        (es,fs) | len > length bs = splitAt (len - length bs) as
                | otherwise       = ([],as)
        rev = reverse (cs ++ es)
        (rs,ss) | len > length bs = splitAt (length cs) rev
                | otherwise       = (rev,[])

sol1' list [] pos skip = (list,pos,skip)
sol1' list (l:ls) pos skip = sol1' list' ls pos' skip'
  where list' = revPiece list l pos
        pos' = (pos + l + skip) `mod` 256
        skip' = skip + 1

sol1 = let (a:b:_, _, _) = sol1' [0..255] input2 0 0
        in a * b


input2 :: [Int]
input2 = map ord "106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118" ++
           [17, 31, 73, 47, 23]

sol2' :: [Int] -> [Int] -> Int -> Int -> Int -> [Int]
sol2' list ls pos skip 0 = dense
  where xs = subLists 16 list
        dense = map (foldr1 xor) xs
sol2' list ls pos skip n = sol2' list' ls pos' skip' (n-1)
  where (list',pos',skip') = sol1' list ls pos skip

subLists n [] = []
subLists n xs = let (ys,zs) = splitAt n xs
                 in ys : subLists n zs

hexstr [] = ""
hexstr (x:xs) = c a : c b : hexstr xs
  where (a,b) = x `divMod` 16
        c n | n < 10    = chr (ord '0' + n)
            | otherwise = chr (ord 'a' + n - 10)

sol2 = hexstr $ sol2' [0..255] input2 0 0 64
