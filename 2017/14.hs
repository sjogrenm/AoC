import Data.Bits
import Data.Char (ord, chr)
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad.ST
import Data.Graph
import Control.Parallel.Strategies
import Data.List

knothash str = f [0..255] 0 0 64
  where ls = map ord str ++ [17,31,73,47,23]
        f :: [Int] -> Int -> Int -> Int -> [Int]
        f list pos skip 0 = dense
          where xs = subLists 16 list
                dense = map (foldr1 xor) xs
        f list pos skip n = f list' pos' skip' (n-1)
          where (list',pos',skip') = g list ls pos skip
        g list [] pos skip = (list,pos,skip)
        g list (l:ls) pos skip = g list' ls pos' skip'
          where list' = revPiece list l pos
                pos' = (pos + l + skip) `mod` 256
                skip' = skip + 1

subLists n [] = []
subLists n xs = let (ys,zs) = splitAt n xs
                 in ys : subLists n zs

revPiece ls len pos
  = ss ++ fs ++ rs ++ ds
  where (as,bs) = splitAt pos ls
        (cs,ds) = splitAt len bs
        (es,fs) | len > length bs = splitAt (len - length bs) as
                | otherwise       = ([],as)
        rev = reverse (cs ++ es)
        (rs,ss) | len > length bs = splitAt (length cs) rev
                | otherwise       = (rev,[])

bitstr :: [Int] -> String
bitstr [] = []
bitstr (i:is) = toBit a ++ toBit b ++ bitstr is
  where (a,b) = i `divMod` 16
        toBit x = [ if s then '1' else '0' | s <- map (testBit x) [3,2,1,0] ]

hexstr [] = ""
hexstr (x:xs) = c a : c b : hexstr xs
  where (a,b) = x `divMod` 16
        c n | n < 10    = chr (ord '0' + n)
            | otherwise = chr (ord 'a' + n - 10)

countBits is = sum (map popCount is)

input = "ljoxqyyw"

hashes inp = parMap rpar knothash [ inp ++ "-" ++ show i | i <- [0..127] ]

sol1 = sum (map countBits $ hashes input)


{-
regions (x:xs) = regions' (x:xs) (replicate (length x) '0')
regions' [] _ = return []
regions' (row:rows) prevRow = do
    thisRow <- mapM [ f x l u | (x, l, u) <- zip3 row ('0' : row) prevRow ]
  where f x l u = 
  -}

toMap xs = f Map.empty 0 xs
  where f m row [] = m
        f m row (x:xs) = f (foldl g m (zip x [0..])) (row+1) xs
          where g m ('1',col) = Map.insert (row,col) 0 m
                g m _ = m

bitstr2 :: [Int] -> [Int]
bitstr2 [] = []
bitstr2 (i:is) = toBit a ++ toBit b ++ bitstr2 is
  where (a,b) = i `divMod` 16
        toBit x = [ if s then 1 else 0 | s <- map (testBit x) [3,2,1,0] ]

{-
foo inp = runST $ V.thaw ls >>= f [0..127] [0..127]
  where ls = V.fromList (concatMap bitstr2 $ hashes inp)
        f [] _ a = return ()
        f (r:rs) cs a = do a' <- g a r cs
                           f rs cs a'
        g a r [] = return a
        g a r (c:cs) = return a
        -}


bits i = map (testBit i) [7,6..0]

bitGrid input = map (concatMap bits) (hashes input)

sol2 = sol2' input

sol2' input = let bg = bitGrid input
                  es = [ ((), (x,y), [ (x-1,y) | left ] ++ [ (x, y-1) | up ])
                       | (y, line, prev) <- zip3 [0..] bg (repeat False : bg)
                       , (x, True, up, left) <- zip4 [0..] line prev (False : line) ]
                  (g, _, _) = graphFromEdges es
               in length (components g)
