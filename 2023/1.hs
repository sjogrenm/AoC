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

import Util

processInput :: String -> [Int]
processInput s = [ read ([head ys, last ys]) | xs <- lines s, let ys = filter isDigit xs ]

input1 = do i <- readFile "1.txt"
            return (processInput i)

input2 = do i <- readFile "1.txt"
            return (lines i)

sol1 xs = sum xs

sol2 xs = sum [ head ys * 10 + last ys | ys <- map findNumbers xs ]

findNumbers :: String -> [Int]
findNumbers [] = []
findNumbers (x:xs) | isDigit x    = read [x] : findNumbers xs
findNumbers ('o':'n':'e':xs)      = 1 : findNumbers ('e':xs)
findNumbers ('t':'w':'o':xs)      = 2 : findNumbers ('o':xs)
findNumbers ('t':'h':'r':'e':'e':xs)      = 3 : findNumbers ('e':xs)
findNumbers ('f':'o':'u':'r':xs)      = 4 : findNumbers xs
findNumbers ('f':'i':'v':'e':xs)      = 5 : findNumbers ('e':xs)
findNumbers ('s':'i':'x':xs)      = 6 : findNumbers xs
findNumbers ('s':'e':'v':'e':'n':xs)      = 7 : findNumbers ('n':xs)
findNumbers ('e':'i':'g':'h':'t':xs)      = 8 : findNumbers ('t':xs)
findNumbers ('n':'i':'n':'e':xs)      = 9 : findNumbers ('e':xs)
findNumbers (_:xs) = findNumbers xs

main = do i <- input1
          print (sol1 i)
          j <- input2
          print (sol2 j)
