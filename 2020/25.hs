import Data.Bits
import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace, traceShow)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.Expr

import Util

pkey1 = 10705932
pkey2 = 12301431

trsub k v = v : trsub k ((k * v) `rem` 20201227)

sol1 = (trsub pkey1 1 !! loop2, trsub pkey2 1 !! loop1)
  where sequence = trsub 7 1
        Just loop1 = elemIndex pkey1 sequence
        Just loop2 = elemIndex pkey2 sequence

-- main = do --ts <- input1
          --mapM_ print ts
          --print (qPutList p1 mkQueue)
        --   print (sol1 p1 p2)
        --   print (sol2 p1 p2)
