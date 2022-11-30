import Data.Maybe
import Data.Ord
import Data.List
import Data.Char
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import Util

input1 = do is <- parseInput p "11.txt"
            return $ mkArray is
  where p xs = mkArray [ read [x] :: Int | x <- xs ]



main = do is <- input1
          print is
