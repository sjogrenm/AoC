import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import Util

data Op = Op String Char String | Val Int | Humn | Op2 Op Char Op
    deriving (Eq,Show)

input = parseInput f "21.txt"

f xs = let (y:ys) = words xs
           op = case ys of
                  [n]     -> Val (read n)
                  [n,[o],m] -> Op n o m
        in (init y, op)

eval table n = case op of
                 Val x -> x
                 Op a o b -> g o (eval table a) (eval table b)
  where Just op = Map.lookup n table
        
g '+' = (+)
g '-' = (-)
g '*' = (*)
g '/' = div

sol1 i = eval (Map.fromList i) "root"

ex = map f $ ["root: pppw + sjmn"
             ,"dbpl: 5"
             ,"cczh: sllz + lgvd"
             ,"zczc: 2"
             ,"ptdq: humn - dvpt"
             ,"dvpt: 3"
             ,"lfqf: 4"
             ,"humn: 5"
             ,"ljgn: 2"
             ,"sjmn: drzm * dbpl"
             ,"sllz: 4"
             ,"pppw: cczh / lfqf"
             ,"lgvd: ljgn * ptdq"
             ,"drzm: hmdt - zczc"
             ,"hmdt: 32"]

{-
(4 + (2 * (x - 3))) / 4  == 150
-}


simplify :: Map.Map String Op -> Op -> Op
simplify table (Op a o b) = let Just a' = Map.lookup a table
                                Just b' = Map.lookup b table
                                a'' = simplify table a'
                                b'' = simplify table b'
                             in case (a'',b'') of
                                  (Val x, Val y) -> Val $ g o x y
                                  _              -> Op2 a'' o b''
simplify _ v = v

invert Humn z = z
-- 3 + z = 5    =>  z = 5 - 3
-- 3 - z = 5    =>  z = 3 - 5
-- 3 * z = 5    =>  z = 5 / 3
-- 3 / z = 5    =>  z = 3 / 5
invert (Op2 (Val x) o expr) z = case o of
                                  '+' -> invert expr (z - x)
                                  '-' -> invert expr (x - z)
                                  '*' -> invert expr (z `div` x)
                                  '/' -> invert expr (x `div` z)
-- z + 3 = 5    =>  z = 5 - 3
-- z - 3 = 5    =>  z = 5 + 3
-- z * 3 = 5    =>  z = 5 / 3
-- z / 3 = 5    =>  z = 5 * 3
invert (Op2 expr o (Val x)) z = case o of
                                  '+' -> invert expr (z - x)
                                  '-' -> invert expr (z + x)
                                  '*' -> invert expr (z `div` x)
                                  '/' -> invert expr (z * x)



sol2 i = case (simplify table a', simplify table b') of
           (Val x, expr) -> invert expr x
           (expr, Val y) -> invert expr y
  where table = Map.fromList [ (a, if a == "humn" then Humn else b) | (a,b) <- i ]
        Just (Op a _ b) = Map.lookup "root" table
        Just a' = Map.lookup a table
        Just b' = Map.lookup b table

main = do i <- input
          print (sol1 i)
          print (sol2 i)