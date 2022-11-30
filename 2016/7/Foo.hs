module Foo where

input = do strs <- readFile "input.txt"
           return $ lines strs

parse "" = []
parse xs = (a,True) : parse' b
  where (a,b) = span (/='[') xs
        parse' "" = []
        parse' ('[':ys) = (c,False) : parse d'
          where (c,d) = span (/=']') ys
                d' = case d of [] -> [] ; (']':zs) -> zs

hasAbba (a:b:c:d:xs) = (a == d && b == c && a /= b) || hasAbba (b:c:d:xs)
hasAbba _ = False

hasTls' [] ok = ok
hasTls' ((xs,True):ys) ok = hasTls' ys (ok || hasAbba xs)
hasTls' ((xs,False):ys) ok | hasAbba xs         = False
                           | otherwise          = hasTls' ys ok

hasTls xs = hasTls' xs False

result1 = do i <- input
             return $ length $ filter (hasTls . parse) i

findABAs (a:b:c:xs) = r ++ findABAs (b:c:xs)
  where r = if a == c then [[a,b,c]] else []
findABAs _ = []

hasSSL xs = any id [ any (bab `matches`) abas | bab <- babs ]
  where babs = concatMap findABAs (hypers xs)
        abas = concatMap findABAs (supers xs)

hypers xs = [ x | (x,False) <- xs ]
supers xs = [ x | (x,True) <- xs ]

[a,b,c] `matches` [d,e,f] = a == e && b == d

result2 = do i <- input
             return $ length $ filter (hasSSL . parse) i
