module Util where

import Data.List
import Data.Array
import Data.Char
import Data.Ord

trim = f . f
  where f = reverse . dropWhile isSpace

digits num = reverse (digits' num)
  where digits' 0 = []
        digits' num = let (a,b) = num `divMod` 10
                       in b : digits' a

parseInput :: (String -> a) -> String -> IO [a]
parseInput f path = do contents <- readFile path
                       return [ f l | l <- lines contents ]

mkArray xs = listArray (0, length xs - 1) xs

x `inBounds` a = let (l,u) = bounds a in x >= l && x <= u

hexstr [] = ""
hexstr (x:xs) = c a : c b : hexstr xs
  where (a,b) = x `divMod` 16
        c n | n < 10    = chr (ord '0' + n)
            | otherwise = chr (ord 'a' + n - 10)

splitOn _ [] = []
splitOn c xs = case span (/=c) xs of
                 (ys,[])   -> [ys]
                 (ys,_:zs) -> ys : splitOn c zs

splitOnBy _ [] = []
splitOnBy f xs = case span (not . f) xs of
                   (ys,[])   -> [ys]
                   (ys,_:zs) -> ys : splitOnBy f zs

data Queue a = MkQueue [a] [a]
        deriving Show

instance Eq a => Eq (Queue a) where
  q1 == q2    = qGetList q1 == qGetList q2

instance Ord a => Ord (Queue a) where
  q1 `compare` q2  = qGetList q1 `compare` qGetList q2

mkQueue :: Queue a
mkQueue = MkQueue [] []

qPut :: a -> Queue a -> Queue a
qPut item (MkQueue ins outs) = MkQueue (item:ins) outs

qPutList :: [a] -> Queue a -> Queue a
qPutList xs q = foldl' (flip qPut) q xs

qFromList xs = MkQueue [] xs

qGet :: Queue a -> (a, Queue a)
qGet (MkQueue ins (item:rest)) = (item, MkQueue ins rest)
qGet (MkQueue ins@(_:_) []) = qGet (MkQueue [] (reverse ins))

qEmpty :: Queue a -> Bool
qEmpty (MkQueue ins outs) = null ins && null outs

qGetList :: Queue a -> [a]
qGetList q | qEmpty q   = []
qGetList q = let (a,q') = qGet q in a : qGetList q'

qSize (MkQueue ins outs) = length ins + length outs


fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a

toMaybe True x = Just x
toMaybe False _ = Nothing

fromRight (Right x) = x


lookupAll x ys = [ z | (y,z) <- ys, x == y ]


count x [] = 0
count x (y:ys) | x == y         = 1 + count x ys
               | otherwise      = count x ys

x `isIn` [] = False
x `isIn` (y:ys) = x == y || x `isIn` ys
