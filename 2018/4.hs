import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Debug.Trace (trace)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as Map

import Util

getInput = parseInput parse "4.txt"

type Date = (Int,Int,Int)
type Time = (Int,Int)

dist :: (Date,Time) -> (Date,Time) -> Int
dist ((_,mo1,d1), (h1,mi1)) ((_,mo2,d2), (h2,mi2))
  | mo1 == mo2  = 60 * (24 * (d1 - d2) + (h1 - h2)) + (mi1 - mi2)

minutes a b@(_,(_,m)) = [ x `mod` 60 | x <- [m,m+1..m+dist a b-1] ]

data Event = BeginShift Int | FellAsleep | WokeUp
        deriving (Eq, Ord, Show)

data Foo = Foo Date Time Event
        deriving (Eq, Ord, Show)

parseDate xs = let [y,m,d] = splitOn '-' xs in (read y, read m, read d)
parseTime xs = let [h,m] = splitOn ':' xs in (read h, read m)

parseEvent ["Guard", '#':id, "begins", "shift"] = BeginShift (read id)
parseEvent ["falls", "asleep"] = FellAsleep
parseEvent ["wakes", "up"] = WokeUp

parse ('[':xs) = Foo (parseDate d) (parseTime t) (parseEvent (words b))
  where [a,' ':b] = splitOn ']' xs
        [d,t] = words a


type Mapping = Map.IntMap (Map.IntMap Int)

--mostAsleep :: Map.IntMap Int -> [Foo] -> Map.IntMap Int
mostAsleep m [] = m
mostAsleep m (Foo _ _ (BeginShift i):xs) = p m i xs
  where p m id (Foo d1 t1 FellAsleep : Foo d2 t2 WokeUp : xs) = p (Map.alter (f (minutes (d2,t2) (d1,t1))) id m) id xs
        p m id ys = mostAsleep m ys
        f dist (Just a) = Just (dist++a)
        f dist Nothing = Just dist

sol1 result = (guard,mins'', guard * head (head mins''))
  where --m = mostAsleep Map.empty xs
        cmp (a,b) (c,d) = compare (length d) (length b)
        --result = MAp.toAscList m
        (guard,mins) = head $ sortBy cmp result
        mins' = group $ sort mins
        mins'' = sortBy (\xs ys -> compare (length ys) (length xs)) mins'


sol2 result = ((c,m,g),m*g)
  where result' = [ (c,m,g) | (g,ms) <- result, let ms' = group (sort ms), mm <- ms', let m = head mm ; c = length mm ]
        f a b = compare b a
        result'' = sortBy f result'
        (c,m,g) = head result''


main = do i <- getInput
          let ss = sort i
              m = mostAsleep Map.empty ss
              result = Map.toAscList m
          print (sol1 result)
          print (sol2 result)
