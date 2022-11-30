import Data.Maybe
import Data.Ord
import Data.List
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed.Mutable as MV
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Tree
import Text.Parsec
import Text.Parsec.Char

import Util
--import qualified IntCode
--import qualified IntCodeM

data Op = Cut Int | Deal Int | Reverse
        deriving Show

-- LinF a b n  represents  \x -> a*x + b mod n
data LinF a = LinF a a a
        deriving Show

eval (LinF a b n) x = (a*x + b) `mod` n

-- a2 * (a1 * x + b1) + b2
-- (a2*a1) * x + (a2*b1+b2)
compose (LinF a1 b1 n1) (LinF a2 b2 n2)
  | n1 == n2    = LinF ((a2*a1) `mod` n1) ((a2*b1+b2) `mod` n1) n1

powF (LinF _ _ n) 0 = LinF 1 0 n          -- f^0 is identity, ie \x -> 1*x + 0
powF f 1 = f
powF f n = case b of
             0 -> compose f' f'
             1 -> compose f (compose f' f')
  where (n', b) = n `divMod` 2
        f' = powF f n'

invert (LinF a b n) = LinF a' b' n
  where a' = pw a (n-2) -- (a ^ (n - 2)) `mod` n
        b' = (-(a' * b)) `mod` n
        pw a 0 = 1
        pw a 1 = a
        pw a k = case b of
                   0 -> a''
                   1 -> (a * a'') `mod` n
          where (k',b) = k `divMod` 2
                a' = pw a k'
                a'' = (a' * a') `mod` n

parseLine :: (Integral a, Read a) => a -> String -> LinF a
parseLine n s = case words s of
                  -- 0   -> n-1    \x -> -x + (n-1)
                  -- 1   -> n-2    aka
                  -- n-1 -> 0      \x -> (-1)*x + (-1) mod n
                  ["deal","into","new","stack"] -> LinF (-1) (-1) n
                  -- a   -> a-n-k  aka  \x -> 1*x + (-k) mod n
                  ["cut",k]                     -> LinF 1 (-read k) n
                  -- a   -> k*a    aka  \x -> k*x + 0 mod n     
                  ["deal","with","increment",k] -> LinF (read k) 0 n

sol1 text = eval (foldr1 compose ops) 2019
  where ops = map (parseLine 10007) text

sol2 text = eval (invert bigShuff) 2020
  where ops = map (parseLine 119315717514047) text
        shuff = foldr1 compose ops
        bigShuff = powF shuff 101741582076661


main = do xs <- lines `fmap` readFile "22.txt"
          print (sol1 xs)
          print (sol2 xs)
