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

type MapInput = (String, [(Int,Int,Int)])

parseMapInput :: [String] -> MapInput
parseMapInput (nameLine:xs) = (mapName, map p xs)
  where [mapName,_] = splitOn ' ' nameLine
        p s = let [a,b,c] = splitOn ' ' s
               in (read a, read b, read c)

input :: IO ([Int], [MapInput])
input = do s <- readFile "5.txt"
           let ls = lines s
               [seedS]:ls' = splitOn "" ls
               _:seeds = splitOn ' ' seedS

           return (map read seeds, map parseMapInput ls')

mkMap1 dest src len s | s >= src && s < src + len       = Just (s - src + dest)
                      | otherwise                       = Nothing

mkMap [] s = s
mkMap ((dest,src,len):rest) s = case mkMap1 dest src len s of
                                  Nothing -> mkMap rest s
                                  Just x  -> x

-- split [a,a+l) into multiple intervals if it overlaps with [b,b+k)
-- [         )
--           [ )
splitInterval (a,l) (b,k) | a+l <= b    = [(a,l)]

-- [         )
--        [      )
splitInterval (a,l) (b,k) | b > a && a+l > b && b <= a+l && a+l <= b+k    = filterEmpty [(a, newL), (a + newL, l - newL)]
  where newL = b - a

--     [         )
-- [   )
splitInterval (a,l) (b,k) | b+k <= a    = [(a,l)]

--     [         )
-- [       )
splitInterval (a,l) (b,k) | b <= a && b+k > a && b+k <= a+l    = filterEmpty [(a, newL), (a + newL, l - newL)]
  where newL = b + k - a

-- [          )
--     [   )
splitInterval (a,l) (b,k) | b > a && b+k < a+l                  = filterEmpty [(a, l1), (a + l1, l2), (a + l1 + l2, l3)]
  where l1 = b - a
        l2 = k
        l3 = l - l1 - l2

splitInterval (a,l) (b,k) = [(a,l)]

filterEmpty xs = [ (x,y) | (x,y) <- xs, y > 0 ]

-- mkMap1' :: Dest -> Src -> Len -> [Range] -> [Range]
mkMap' maps [] = []
mkMap' maps ((s,l):rs) = mkMap'' (s,l) maps ++ mkMap' maps rs

mkMap'' sl [] = [sl]
mkMap'' sl ((dest,src,len):rest)
  = case is of
      -- no overlap or total overlap
      [(a,l)] | a >= src+len || a+l < src   -> mkMap'' sl rest
              | otherwise                   -> [(a - src + dest, l)]
      -- partial overlap
      [(a,l),(b,k)] | b == src      -> mkMap'' (a,l) rest ++ [(dest, k)]
                    | otherwise     -> (a - src + dest, l) : mkMap'' (b,k) rest
      [(a,l),(b,k),(c,m)]           -> mkMap'' (a,l) rest ++ [(dest, k)] ++ mkMap'' (c,m) rest
  where is = splitInterval sl (src,len)



sol1 (seeds, inputs) = minimum locs
  where maps = map (mkMap . snd) inputs
        locs = map (applyMaps maps) seeds

applyMaps [] x = x
applyMaps (m:ms) x = applyMaps ms (m x)


sol2 (seedDesc, inputs) = minimum $ map fst locs
  where maps = map (mkMap' . snd) inputs
        seeds = mkSeeds seedDesc
        locs = concat [ applyMaps maps [s] | s <- seeds ]

mkSeeds [] = []
mkSeeds (a:b:cs) = (a,b) : mkSeeds cs

main = do i <- input
        --   print i
          print (sol1 i)
        --   j <- input2
          print (sol2 i)
          return ()




{-

[a..b)
->


-}