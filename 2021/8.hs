import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import Util

input1 :: IO [([String],[String])]
input1 = parseInput p "8.txt"
p s = let [xs,ys] = splitOn "|" (words s) in (xs,ys)

digit s = case length s of
            2  -> Just 1
            3  -> Just 7
            4  -> Just 4
            7  -> Just 8
            _  -> Nothing

sol1 xs = length [ s | ss <- ys, s <- ss, isJust $ digit s ]
  where ys = map snd xs

-- miswired -> real
initialCandidates = Map.fromList [ (x,"abcdefg") | x <- "abcdefg" ]


eliminate s candidates = case length s of
                            -- keep !xy -> abdeg
                            -- keep xy -> cf
                            2 -> foldr (Map.adjust (`intersect` "abdeg")) (foldr (Map.adjust (`intersect` "cf")) candidates s) ("abcdefg" \\ s)

                            -- keep !xyz -> bdeg
                            -- keep xyz -> acf
                            3 -> foldr (Map.adjust (`intersect` "bdeg")) (foldr (Map.adjust (`intersect` "acf")) candidates s) ("abcdefg" \\ s)

                            -- keep !xyzw -> aeg
                            -- keep xyzw -> bcdf
                            4 -> foldr (Map.adjust (`intersect` "aeg")) (foldr (Map.adjust (`intersect` "bcdf")) candidates s) ("abcdefg" \\ s)

                            _ -> candidates

-- 2seg     1       cf
-- 3seg     7       acf
-- 4seg     4       bcdf
-- 5seg     235     acdeg acdfg abdfg
-- 6seg     069     abcefg abdefg abcdfg
-- 7seg     8

foo ex n = case length n of
             2 -> 1
             3 -> 7
             4 -> 4
             5 -> if n' == d5 then 5 else if n' == d3 then 3 else 2
             6 -> if n' == d6 then 6 else if n' == d0 then 0 else 9
             7 -> 8
  where n' = sort n
        [d1] = [ sort x | x <- ex, length x == 2 ]
        [d7] = [ sort x | x <- ex, length x == 3 ]
        [d4] = [ sort x | x <- ex, length x == 4 ]
        [d8] = [ sort x | x <- ex, length x == 7 ]
        [d6] = [ sort x | x <- ex, length x == 6, length (d1 \\ x) == 1 ]
        [d0] = [ sort x | x <- ex, length x == 6, length (d4 \\ x) == 1, sort x /= d6 ]
        [d9] = [ sort x | x <- ex, length x == 6, sort x `notElem` [d0,d6] ]
        [d5] = [ sort x | x <- ex, length x == 5, length (d6 \\ x) == 1 ]
        [d3] = [ sort x | x <- ex, length x == 5, length (d9 \\ x) == 1, sort x /= d5 ]


sol2 is = sum [ toNum $ map (foo ex) ns | (ex,ns) <- is ]

toNum xs = toNum' xs 0
toNum' [] y = y
toNum' (x:xs) y = toNum' xs (y*10 + x)

ex1625 = p "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"

example = map p
          [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
          , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
          , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
          , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
          , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
          , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
          , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
          , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
          , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
          , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
          ]

-- toNum [a,b,c] -> c + 10*(b + 10*a)

main = do xs <- input1
          --print (map snd xs)
          --print xs
          --print (sol1 xs)
          print $ sol2 xs
          --print (sol2 xs)
