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

input1 = do contents <- readFile "4.txt"
            let xs = [ concatMap words y | y <- splitOn "" (lines contents) ]
            return $ map f xs
  where f xs = [ (a,b) | x <- xs, let [a,b] = splitOn ':' x ]

numOfLen :: Int -> String -> Maybe Int
numOfLen n xs | length xs == n && all isDigit xs        = Just (read xs)
              | otherwise                               = Nothing

requiredFields = [
                  ("byr", \x -> maybe False (\y -> y >= 1920 && y <= 2002) (numOfLen 4 x)),
                  ("iyr", \x -> maybe False (\y -> y >= 2010 && y <= 2020) (numOfLen 4 x)),
                  ("eyr", \x -> maybe False (\y -> y >= 2020 && y <= 2030) (numOfLen 4 x)),
                  ("hgt", \x -> case splitAt 2 (reverse x) of
                                  ("mc",y) -> let z = reverse y
                                               in all isDigit z && read z >= 150 && read z <= 193
                                  ("ni",y) -> let z = reverse y
                                               in all isDigit z && read z >= 59 && read z <= 76
                                  _        -> False),
                  ("hcl", \x -> head x == '#' && all isHexDigit (tail x)),
                  ("ecl", \x -> x `isIn` ["amb","blu","brn","gry","grn","hzl","oth"]),
                  ("pid", \x -> isJust (numOfLen 9 x))
                  ]

sol1filter xs = [ x | x <- xs, map fst requiredFields \\ map fst x == [] ]
sol1 xs = length (sol1filter xs)

sol2filter xs = [ x | x <- xs, all f x ]
  where f (f,v) = case lookup f requiredFields of
                    Just func -> func v
                    Nothing   -> f == "cid"

sol2 xs = length (sol2filter (sol1filter xs))

main = do i <- input1
          --print i
          print (sol1 i)
          print (sol2 i)
