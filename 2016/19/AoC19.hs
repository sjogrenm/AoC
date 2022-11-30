module Main where

import Debug.Trace (trace)

elves = 3017957

input n = [ (elf, 1) | elf <- [1..n] ]

result1 [x] = x
result1 ((elf,p):(_,q):zs) = result1 (zs ++ [(elf,p+q)])

result1' q | queueEmpty q'      = (elf,p1)
           | otherwise          = result1' (queuePut (elf,p1+p2) q'')
  where ((elf,p1),q') = queueGet q
        ((_,p2),q'') = queueGet q'


--main = print $ result2 $ queueInit $ input 3017957


data Queue a = MkQueue Int [a] [a]
        deriving Show

mkQueue :: Queue a
mkQueue = MkQueue 0 [] []

queueInit xs = MkQueue (length xs) [] xs

queuePut :: a -> Queue a -> Queue a
queuePut item (MkQueue sz ins outs) = MkQueue (sz+1) (item:ins) outs

--queuePutList :: [a] -> Queue a -> Queue a
--queuePutList xs q = foldl' (flip queuePut) q xs

queueGet :: Queue a -> (a, Queue a)
queueGet (MkQueue sz ins (item:rest)) = (item, MkQueue (sz-1) ins rest)
queueGet (MkQueue sz ins []) = queueGet (MkQueue sz [] (reverse ins))

queueEmpty :: Queue a -> Bool
queueEmpty (MkQueue _ ins outs) = null ins && null outs

queueSize :: Queue a -> Int
queueSize (MkQueue sz _ _) = sz



queueGetAt (MkQueue sz ins outs) i
  = case ys of
      z:zs -> (z, MkQueue (sz-1) ins (xs++zs))
      _    -> let i' = length ins - (i - length outs) - 1
                  (xs,y:ys) = splitAt i' ins
               in (y, MkQueue (sz-1) (xs++ys) outs)
  where (xs,ys) = splitAt i outs


result2 q | queueEmpty (trace (show $ queueSize q') q')       = (elf,p1)
          | otherwise           = result2 (queuePut (elf,p1+p2) q'')
  where ((elf,p1),q') = queueGet q
        ((_,p2),q'') = queueGetAt q' ((queueSize q' - 1) `div` 2)


data S a = S (Queue a) (Queue a)
        deriving Show

sInit xs = S (queueInit ys) (queueInit zs)
  where (ys,zs) = splitAt (length xs `div` 2) xs

{-
sSingle :: S a -> Bool
sSingle (S q1 q2) = queueSize q1 == 1 && queueEmpty q2

sHead :: S a -> a
sHead (S q1 _) = fst (queueGet q1)

sMid :: S a -> a
sMid (S _ q2) = fst (queueGet q2)
-}

-- turns 1|234|5678 into 2|34|6781
solve :: S (a,Int) -> (a,Int)
solve (S q1 q2) | queueEmpty (
                        {-trace (show (queueSize q1, queueSize q2))-}
                        q1)         = fst (queueGet q2)
solve (S q1 q2) = if queueSize q1' == queueSize q2'
                    then solve (S q1' (queuePut (elf,p1+p2) q2'))
                    else let (x,q2'') = queueGet q2'
                          in solve (S (queuePut x q1') (queuePut (elf,p1+p2) q2''))
  where ((elf,p1),q1') = queueGet q1
        ((_,p2),q2') = queueGet q2



main = print $ solve2 elves


solve2 n = solve' (queueInit [1..k]) (queueInit [k+1..n])
  where k = n `div` 2
solve' q1 q2 | queueEmpty q1                    = fst (queueGet q2)
             | queueSize q1 == queueSize q2     = solve' q1' (queuePut x q2')
             | otherwise                        = solve' (queuePut y q1') (queuePut x q2'')
  where (x,q1') = queueGet q1
        (_,q2') = queueGet q2
        (y,q2'') = queueGet q2'


{-
1|234  5|678
2|34   6|781
3|47   8|12
4|7    1|23
7|2    3|4
2|     4|7
4|     2|
|      4|

-}
