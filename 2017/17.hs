import Data.Char
import Data.List
import Data.List.Split
import Data.Ord
import Data.Array.Unboxed
import Control.DeepSeq

input :: Int
input = 335

data CircBuffer a = CB { cbSize :: Int, cbElems :: [a] }

instance Show a => Show (CircBuffer a) where
    show (CB _ elems) = show elems

ins pos val (CB sz elems) = CB (sz+1) (xs ++ [val] ++ ys)
  where (xs,ys) = splitAt pos elems


initial = CB 1 [0]
step k (pos, val, cb) = (pos', val+1, ins pos' val cb)
  where pos' = ((pos + k) `mod` cbSize cb) + 1

calc i = [ cb | (pos,val,cb) <- iterate (step i) (0,1,initial) ]

sol1 i = let (2017:x:_) = dropWhile (/=2017) (cbElems $ calc i !! 2017)
          in x


apply 1 f = f
apply n f = case n `divMod` 2 of
              (h, 0) -> apply h (f . f)
              (h, 1) -> apply h (f . f) . f


step' k (pos, sz, currSol) = (pos', sz+1, sol' `deepseq` sol')
  where pos' = ((pos + k) `mod` sz) + 1
        sol' = if pos' == 1 then sz else currSol

sol2 k = sol2' k (0,1,0)

sol2' k (pos, 50000001, currSol) = currSol
sol2' k (pos, sz, currSol) = sol2' k (pos', sz+1, sol' `deepseq` sol')
  where pos' = ((pos + k) `mod` sz) + 1
        sol' = if pos' == 1 then sz else currSol


main = print (sol2 input)

{-
(0+3) % 1 + 1   -> 1
(1+3) % 2 + 1   -> 1
(1+3) % 3 + 1   -> 


-}
