import Data.Bits
import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace, traceShow)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.Expr

import Util

input1 = parseInput p "21.txt"

p s = let xs = splitOn '(' s
       in case xs of
            [a] -> (words a, [])
            [a,b] -> (words a, g b)
  where g ('c':'o':'n':'t':'a':'i':'n':'s':' ':s) = map trim $ splitOn ',' (init s)

ex = [p "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)", p "trh fvjkl sbzzf mxmxvkd (contains dairy)", p "sqjhc fvjkl (contains soy)", p "sqjhc mxmxvkd sbzzf (contains fish)"]

ingredientsWithAllergens m = Set.fromList (concat $ Map.elems m)

allergenToIngredient xs = buildMap xs Map.empty
  where buildMap [] m = m
        buildMap ((is,as):xs) m = let f a = Map.insertWith intersect a is
                                   in buildMap xs (foldr f m as)

sol1 xs = length [ i | (is,_) <- xs, i <- is, not (i `Set.member` ia) ]
  where m = allergenToIngredient xs
        ia = ingredientsWithAllergens m

sol2 xs = concat $ intersperse "," [ i | (a,[i]) <- Map.toAscList $ f as m ]
  where m = allergenToIngredient xs
        as = Map.keys m
        f [] m = m
        f (a:as) m = case Map.lookup a m of
                       Just [i] -> let g = Map.adjust (\is -> is \\ [i]) in f as (foldr g m as)
                       Just _   -> f (as++[a]) m

main = do ts <- input1
          --mapM_ print ts
          print (sol1 ts)
          print (sol2 ts)
