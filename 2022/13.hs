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

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator

import Util

data List = LInt Int | LList [List]
    deriving Eq

instance Show List where
    show (LInt i) = show i
    show (LList xs) = show xs

instance Ord List where
    compare (LInt x)   (LInt y)   = compare x y
    compare (LList xs) (LList ys) = compare xs ys
    compare x          (LList ys) = compare (LList [x])  (LList ys)
    compare (LList xs) y          = compare (LList xs) (LList [y])

readL s = case parse q "read" s of
            Left err -> error (show err)
            Right xs -> xs
  where p = between (char '[') (char ']') (q `sepBy` char ',')
        q = fmap LInt integer <|> fmap LList p

integer :: Monad m => ParsecT String u m Int
integer = do xs <- many1 digit
             return (read xs)

input :: IO [(List,List)]
input = do c <- readFile "13.txt"
           let ls = splitOn [] (lines c)
           return [ (readL x,readL y) | [x,y] <- ls ]


sol1 xs = sum [ if x < y then i else 0 | ((x,y),i) <- zip xs [1..] ]


sol2 xs = (i1 + 1) * (i2 + 1)
  where ys = sort $ [p1, p2] ++ concat [ [x,y] | (x,y) <- xs ]
        p1 = readL "[[2]]"
        p2 = readL "[[6]]"
        Just i1 = elemIndex p1 ys
        Just i2 = elemIndex p2 ys

main = do i <- input
          print (sol1 i)
          print (sol2 i)

ex = map readL
  [ "[1,1,3,1,1]"
  , "[1,1,5,1,1]"
  , "[[1],[2,3,4]]"
  , "[[1],4]"
  , "[9]"
  , "[[8,7,6]]"
  , "[[4,4],4,4]"
  , "[[4,4],4,4,4]"
  , "[7,7,7,7]"
  , "[7,7,7]"
  , "[]"
  , "[3]"
  , "[[[]]]"
  , "[[]]"
  , "[1,[2,[3,[4,[5,6,7]]]],8,9]"
  , "[1,[2,[3,[4,[5,6,0]]]],8,9]"
  ]