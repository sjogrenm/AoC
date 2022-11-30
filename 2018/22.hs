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
import Data.Bits
import Text.Parsec
import Text.Parsec.Char


import Util


depth :: Int
depth = 5355

target :: (Int,Int)
target = (14,796)

ix (x,y) = 1000*x + y

erosion :: Int -> (Int,Int) -> V.Vector Int
erosion depth t@(tx,ty) = V.create $ do
        mv <- MV.replicate (1000*1000) (-1)
        let geoIndex (0,0) = return 0
            geoIndex (x,0) = return $ x * 16807
            geoIndex (0,y) = return $ y * 48271
            geoIndex (x,y) | (x,y) == t   = return 0
                           | otherwise    = do e1 <- MV.read mv (ix (x-1,y))
                                               e2 <- MV.read mv (ix (x,y-1))
                                               return $ e1 * e2
        sequence_ [ do gi <- geoIndex (x,y)
                       MV.write mv (ix (x,y)) ((gi + depth) `mod` 20183)
                  | x <- [0..tx],
                    y <- [0..ty]
                  ]
        return mv


sol1 depth t@(tx,ty) = sum [ (e ! (ix (x,y))) `mod` 3 | x <- [0..tx], y <- [0..ty] ]
  where e = erosion depth t

