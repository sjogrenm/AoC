import Data.Maybe
import Data.Ord
import Data.List
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Tree
import Text.Parsec
import Text.Parsec.Char

import Util
import qualified IntCode
import qualified IntCodeM

type Reaction = (String, (Int, [(String, Int)]))

parseLine :: String -> Reaction
parseLine xs = y
  where Right y = parse p "14.txt" xs
        p = do xs <- sepBy1 q (string ", ")
               string " => "
               (v,n) <- q
               return (v, (n, xs))
        q = do n <- read `fmap` many1 digit
               spaces
               v <- many1 letter
               return (v,n)


input1 :: IO [Reaction]
input1 = parseInput parseLine "14.txt"

example1, example2 :: [Reaction]
example1 = map parseLine ["10 ORE => 10 A","1 ORE => 1 B", "7 A, 1 B => 1 C", "7 A, 1 C => 1 D", "7 A, 1 D => 1 E", "7 A, 1 E => 1 FUEL"]
example2 = map parseLine ["9 ORE => 2 A", "8 ORE => 3 B", "7 ORE => 5 C", "3 A, 4 B => 1 AB", "5 B, 7 C => 1 BC", "4 C, 1 A => 1 CA", "2 AB, 3 BC, 4 CA => 1 FUEL"]
example3 = map parseLine ["157 ORE => 5 NZVS" , "165 ORE => 6 DCFZ" , "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL" , "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ" , "179 ORE => 7 PSHF" , "177 ORE => 5 HKGWZ" , "7 DCFZ, 7 PSHF => 2 XJWVT" , "165 ORE => 2 GPVTF" , "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"]
example4 = map parseLine ["171 ORE => 8 CNZTR" , "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL" , "114 ORE => 4 BHXH" , "14 VRPVC => 6 BMBT" , "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL" , "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT" , "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW" , "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW" , "5 BMBT => 4 WPTQ" , "189 ORE => 9 KTJDG" , "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP" , "12 VRPVC, 27 CNZTR => 2 XDBXC" , "15 KTJDG, 12 BHXH => 5 XCVML" , "3 BHXH, 2 VRPVC => 7 MZWV" , "121 ORE => 7 VRPVC" , "7 XCVML => 6 RJRHP" , "5 BHXH, 4 VRPVC => 5 LTCX"]


sol1 reactions [] accOre spares = accOre
sol1 reactions (("ORE",n):rs) accOre spares = sol1 reactions rs (accOre+n) spares
sol1 reactions ((v,n):rs) accOre spares
  | spareV >= n         = sol1 reactions ({-trace (show (v,n) ++ ": used spares")-} rs) accOre (Map.alter update1 v spares)
  | otherwise           = sol1 reactions ({-trace (show (v,n) ++ ": " ++ show reqs' ++ " (" ++ show spareV' ++ " to spare)")-} reqs'++rs) accOre (Map.alter update2 v spares)
  where spareV = fromMaybe 0 $ Map.lookup v spares
        n' = n - spareV
        update1 m = ifPositive (fromMaybe 0 m - n)
        update2 m = ifPositive spareV' --(fromMaybe 0 (update1 m) + spareV')
        Just (rvn, reqs) = Map.lookup v reactions
        (reqs',spareV') = getReqs n' (rvn, reqs)

getReqs n' (rvn, reqs) = ([ (v, n * (x+y1)) | (v,n) <- reqs ], y2)
  where (x,y) = n' `divMod` rvn
        y1 = if y == 0 then 0 else 1
        y2 = if y == 0 then 0 else rvn - y

multReqs m xs = [ (v,m*n) | (v,n) <- xs ]

a `ceilDiv` b = x + if y == 0 then 0 else 1
  where (x,y) = a `divMod` b

ifPositive n | n > 0    = Just n
ifPositive n | n < 0    = error "argh"
ifPositive _            = Nothing

sol2 i = head [ (k, foo)
              | k <- [3062000,3061999..],
                let foo = sol1 (Map.fromList i) [("FUEL", k)] 0 Map.empty,
                foo < 1000000000000 ]

main = do i <- input1
          print $ sol1 (Map.fromList i) [("FUEL", 1)] 0 Map.empty
          --print $ sol1 (Map.fromList i) [("FUEL", 3050000)] 0 Map.empty
          print $ sol2 i
