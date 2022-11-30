import Data.Char (chr, ord)
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

import Util
import qualified IntCode
--import qualified IntCodeM

input1 :: IO [Int]
input1 = do [ls] <- lines `fmap` readFile "21.txt"
            return $ map read $ splitOn ',' ls


sol1 p = IntCode.runIO p script
  where script = map ord $ unlines [ "NOT A J"
                                   , "NOT B T"
                                   , "OR T J"
                                   , "NOT C T"
                                   , "OR T J"
                                   , "AND D J"
                                   , "WALK" ]

sol2 p = IntCode.runIO p script
  where script = map ord $ unlines [ "NOT A J"
                                   , "NOT B T"
                                   , "OR T J"
                                   , "NOT C T"
                                   , "OR T J"
                                   , "AND D J"
                                   , "NOT J T"
                                   , "OR E T"
                                   , "OR H T"
                                   , "AND T J"
                                   , "RUN" ]

main = do i <- input1
          let p = UV.fromList i
          --putStrLn $ map chr $ sol1 p
          --print $ sol1 p
          let (output,score) = span (<255) $ sol2 p
          putStrLn $ map chr output
          print score

{-

(H | E) & D & (not A | not B | not C)


D | B

- (-D & -B)


-}
