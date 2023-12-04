import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Util


input = parseInput f "4.txt"

data Card = Card [Int] [Int]
    deriving Show

f :: String -> Card
f s = fromRight $ runParser p () "4.txt" s

p = do string "Card"
       spaces
       int
       char ':'
       spaces
       wins <- many1 $ do i <- int
                          spaces
                          return i
       char '|'
       nums <- many1 (spaces >> int)
       return $ Card wins nums

int = do s <- many1 digit
         return (read s :: Int)


winnings card | m > 0       = 2 ^ (m - 1)
              | otherwise   = 0
  where m = matches card

matches (Card wins nums) = length $ nums `intersect` wins

sol1 cards = sum (map winnings cards)

sol2 cards = sol2' 0 (zip cards (repeat 1))

sol2' acc [] = acc
sol2' acc cards = let (n, cs) = iter cards
                   in sol2' (acc + n) cs
                

iter ((c,n):cards) | m > 0      = let (xs,ys) = splitAt m cards
                                      xs' = [ (d,k+n) | (d,k) <- xs ]
                                   in (n, xs' ++ ys)
                   | otherwise  = (n, cards)
  where m = matches c

main = do i <- input
        --   print i
        --   print (findThings i)
        --   print i
          print (sol1 i)
        --   j <- input2
          print (sol2 i)


ex1 = Card [41,48,83,86,17] [83,86,6,31,17,9,48,53]
ex2 = f "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
ex3 = f "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
ex4 = f "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
ex5 = f "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
ex6 = f "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

ex = [ex1,ex2,ex3,ex4,ex5,ex6]