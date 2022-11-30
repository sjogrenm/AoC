import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
--import Data.Array
import Debug.Trace (trace, traceShow)
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies
import Data.Graph as G

import Util

input =
    [ "################################"
    , "#...############################"
    , "###G.###########################"
    , "##.....#########################"
    , "#......#########################"
    , "##G...G.########################"
    , "#G.....G########################"
    , "###...G#########################"
    , "###....#########################"
    , "######.G.#######################"
    , "#######....#####################"
    , "###..#.....GG...G.E...##########"
    , "##........G...#####...##.#######"
    , "#.G..........#######...#..######"
    , "#...####G...#########......#####"
    , "#..G##.#..G.#########.......####"
    , "#...##....E.#########...E.....##"
    , "#...##......#########G......####"
    , "#...........#########.......####"
    , "#............#######...........#"
    , "#.....E..G...E#####E...........#"
    , "#.G...........G.............E###"
    , "#...............E#####.#..######"
    , "#..#..G...........####...#######"
    , "#..#..............######.#######"
    , "####.#...E.......###############"
    , "########..##...#################"
    , "##...##..###..##################"
    , "#.......########################"
    , "##...E..########################"
    , "###......#######################"
    , "################################"
    ]

data Unit = U Char (Int,Int) Int
        deriving (Eq, Show)

race (U r _ _) = r

sortUnits = sortBy (comparing f)
  where f (U _ (x,y) _) = (y,x)

processInput :: Int -> [String] -> ([Unit], [String])
processInput _ [] = ([], [])
processInput y (a:as) = (us1++us2, a':as')
  where (us1, a') = processLine 0 a
        (us2, as') = processInput (y+1) as
        processLine _ [] = ([], [])
        processLine x (b:bs) | b `elem` "GE"    = (U b (x,y) 200 : us, '.':bs')
                             | otherwise        = (us, b:bs')
          where (us, bs') = processLine (x+1) bs


iter gen cave moved [] = (gen+1, sortUnits moved)
iter gen cave moved (u:us)
  | null targets        = (gen, sortUnits (moved++(u:us))) -- combat done
  where targets = [ e | e@(U r _ _) <- moved++us, r /= race u ]

sol1 i = caveG
  where (units, i') = processInput 0 i
        cave = V.fromList (map V.fromList i')
        (caveG,nfv,vfk) = graphFromEdges edges
        edges = mkGraph 1 (map V.fromList i')

mkGraph :: Int -> [V.Vector Char] -> [((), (Int,Int), [(Int,Int)])]
mkGraph y (a:b:c:ds) = [ ((), (x,y), adj x) | (x,'.') <- zip [0..] (V.toList b) ] ++ mkGraph (y+1) (b:c:ds)
  where adj x = concat [ f a x (y-1), f c x (y+1), f b (x-1) y, f b (x+1) y ]
        f v x y | v!x == '.'    = [(x,y)]
                | otherwise     = []
mkGraph _ _ = []



