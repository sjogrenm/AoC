import Debug.Trace
import Data.Char
import Data.Ord
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Array
import qualified Data.Set as S
import qualified Data.Map.Strict as M

parseInput = do i <- readFile "24input.txt"
                return $ map parseStr $ lines i

parseStr :: String -> [Int]
parseStr s = map read (splitOn "/" s)


sample = map parseStr $ words "0/2 2/2 2/3 3/4 3/5 0/1 10/1 9/10"

paths unused curr@(x:_)
  = case cand of
      [] -> [curr]
      _  -> concat [ paths (cand'++unused') (y:x:curr)
                   | c@[a,b] <- cand, let cand' = cand \\ [c], let y = if a == x then b else a ]
  where (cand, unused') = partition (x`elem`) unused

sol1' xs = maximumBy (comparing sum) $ paths xs [0]

sol1 = do i <- parseInput
          let path = sol1' i
          return (sum path, path)

sol2' xs = maximumBy (comparing sum) (p : takeWhile (\x -> length x == length p) ps)
  where p:ps = sortBy (comparing (Down . length)) $ paths xs [0]

sol2 = do i <- parseInput
          let path = sol2' i
          return (sum path, path)
