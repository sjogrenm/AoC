module Main where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Char (chr, ord)
import Data.List
import Data.Word
import Debug.Trace (trace)

data Context = C (Int,Int) MD5.Ctx [Word8]

instance Eq Context where
  C _ _ p1 == C _ _ p2  = p1 == p2

instance Ord Context where
  C _ _ p1 <= C _ _ p2  = p1 <= p2

instance Show Context where
  show (C pos _ path) = "<At " ++ show pos ++ " after " ++ show (showPath $ reverse path) ++ ">"

input = "hhhxzeay"

u, d, l, r :: Word8
u = fromIntegral (ord 'U')
d = fromIntegral (ord 'D')
l = fromIntegral (ord 'L')
r = fromIntegral (ord 'R')
exitNames = [u, d, l, r]

a, z :: Word8
a = fromIntegral $ ord 'a'
z = fromIntegral $ ord '0'

exits :: MD5.Ctx -> [Bool]
exits ctx = [u >= 10, d >= 10, l >= 10, r >= 10]
  where s = MD5.finalize ctx
        (u,d) = (s `B.index` 0) `divMod` 16
        (l,r) = (s `B.index` 1) `divMod` 16

validExits :: Context -> [Bool]
validExits (C (x,y) ctx _) = [u && y > 0, d && y < 3, l && x > 0, r && x < 3]
  where [u,d,l,r] = exits ctx

moves :: Context -> [Word8]
moves ctx = [ eName | (eName,ex) <- zip exitNames (validExits ctx), ex ]

mkCtx = MD5.update MD5.init . C8.pack

move :: Word8 -> Context -> Context
move dir (C (x,y) ctx path) = C pos' ctx' (dir:path)
  where ctx' = MD5.update ctx (B.singleton dir)
        pos' | dir == u = (x,y-1)
             | dir == d = (x,y+1)
             | dir == l = (x-1,y)
             | dir == r = (x+1,y)

initial seed = C (0,0) (mkCtx seed) []

main = print $ dfs [initial "ihgpwlah"] 0
--main = print $ bfs2 (queuePut (C (0,0) (mkCtx "ihgpwlah") []) mkQueue) [] 0

showPath = map (chr . fromIntegral)

dfs []     ls = ls
dfs (c@(C (3,3) _ str):cs) ls = --trace (show $ length cs) $
                                --trace (show $ length str) $
                                trace (show c) $
                                if length str > 400 then error (show c)
                                  else dfs cs ls'
  where ls' = max (length str) ls
dfs (c@(C (x,y) _ _):_) _ | x < 0       = error "x < 0"
                          | y < 0       = error "y < 0"
                          | x > 3       = error "x > 3"
                          | y > 3       = error "y > 3"
dfs (c@(C pos _ str):cs) ls = --trace (show (length cs)) $
                              dfs cs' ls
  where ms = moves c
        cs' = [ move m c | m <- ms ] ++ cs

debug x = trace (show x) x


bfs2 q v ls | queueEmpty ({-trace (show $ queueSize q)-} q)        = ls
            | fst pos < 0 || fst pos > 3  = error (show pos)
            | snd pos < 0 || snd pos > 3  = error (show pos)
--            | ctx `elem` v                = error "revisited state"
            | ls > 400  = error (show ctx)
            | otherwise           = case pos of
                                      (3,3) -> --trace (show $ length str) $
                                               bfs2 q' (ctx:v) ls'
                                      _     -> bfs2 q'' (ctx:v) ls
  where (ctx@(C pos _ str), q') = queueGet q
        ms = moves ctx
        q'' = foldr (\m q -> queuePut (move m ctx) q) q' ms
        ls' = max ls (length str)


--bfs input = bfs' (queuePut (initial input) mkQueue)
bfs input = bfs2 (queuePut (initial input) mkQueue) [] 0

bfs' q | queueEmpty q   = undefined
       | pos == (3,3)   = ctx
       | otherwise      = bfs' q''
  where (ctx@(C pos _ str), q') = queueGet q
        ms = moves ctx
        q'' = foldr (\m q -> queuePut (move m ctx) q) q' ms



data Queue a = MkQueue [a] [a]

mkQueue :: Queue a
mkQueue = MkQueue [] []

queuePut :: a -> Queue a -> Queue a
queuePut item (MkQueue ins outs) = MkQueue (item:ins) outs

queuePutList :: [a] -> Queue a -> Queue a
queuePutList xs q = foldl' (flip queuePut) q xs

queueGet :: Queue a -> (a, Queue a)
queueGet (MkQueue ins (item:rest)) = (item, MkQueue ins rest)
queueGet (MkQueue ins []) = queueGet (MkQueue [] (reverse ins))

queueEmpty :: Queue a -> Bool
queueEmpty (MkQueue ins outs) = null ins && null outs

queueSize :: Queue a -> Int
queueSize (MkQueue xs ys) = length xs + length ys



shortestPath seed = minimumBy (\x y -> compare (length x) (length y)) $ allPaths seed

allPaths seed = go "" (0,0)
  where go path pos = do
                (pos', dir) <- getNeighbours pos . findOpen $ seed++path
                let path' = path++[dir]
                if pos' == (3,3)
                  then return path'
                  else go path' pos'
getNeighbours (x,y) doors = [ (n, dir) | dir <- doors, let n = step dir, valid n ]
  where step 'U' = (x, y-1)
        step 'D' = (x, y+1)
        step 'L' = (x-1, y)
        step 'R' = (x+1, y)
        valid (x,y) = all (`elem` [0..3]) [x,y]
--findOpen path = map ("UDLR" !!) . findIndices (>= 'b') . take 4 . hash
findOpen s = [ ex | (True, ex) <- zip (exits (mkCtx s)) "UDLR" ]
