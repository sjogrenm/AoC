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

-- Time:        47     98     66     98
-- Distance:   400   1213   1011   1540

dist totT butT = (totT - butT) * butT

input1 :: [(Int,Int)]
input1 = [(47, 400), (98, 1213), (66, 1011), (98, 1540)]

input2 :: [(Int,Int)]
input2 = [(47986698, 400121310111540)]

sol1 :: [(Int,Int)] -> Int
sol1 is = product [ length [ butT | butT <- [1..totT-1], dist totT butT > targetD ] | (totT,targetD) <- is ]

main = do -- i <- input
        --   print i
          print (sol1 input1)
        --   j <- input2
          print (sol1 input2)
          return ()


