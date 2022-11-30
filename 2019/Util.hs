module Util where

import Data.List
import Data.Array
import Data.Char
import Data.Ord

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

splitOn _ "" = []
splitOn c xs = case span (/=c) xs of
                 (ys,[])   -> [ys]
                 (ys,_:zs) -> ys : splitOn c zs

data Queue a = MkQueue [a] [a]
        deriving Show

mkQueue :: Queue a
mkQueue = MkQueue [] []

qPut :: a -> Queue a -> Queue a
qPut item (MkQueue ins outs) = MkQueue (item:ins) outs

qPutList :: [a] -> Queue a -> Queue a
qPutList xs q = foldl' (flip qPut) q xs

qGet :: Queue a -> (a, Queue a)
qGet (MkQueue ins (item:rest)) = (item, MkQueue ins rest)
qGet (MkQueue ins []) = qGet (MkQueue [] (reverse ins))

qEmpty :: Queue a -> Bool
qEmpty (MkQueue ins outs) = null ins && null outs


fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a

toMaybe True x = Just x
toMaybe False _ = Nothing

fromRight (Right x) = x


lookupAll x ys = [ z | (y,z) <- ys, x == y ]
