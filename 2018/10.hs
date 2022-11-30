import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
--import Data.Array
import Debug.Trace (trace)
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as Map
import Text.Parsec
import Text.Parsec.Char

import Util


getMyInput :: IO [((Int,Int),(Int,Int))]
getMyInput = parseInput (fromRight . runParser pars () "stuff") "10.txt"

integer = do x <- optionMaybe (char '-')
             xs <- many1 digit
             return (read $ maybe "" (:[]) x ++ xs)

pars = do string "position=<"
          spaces
          px <- integer
          char ','
          spaces
          py <- integer
          string "> velocity=<"
          spaces
          vx <- integer
          char ','
          spaces
          vy <- integer
          char '>'
          return ((px,py), (vx,vy))

iter xs = [ ((px+vx,py+vy),(vx,vy)) | ((px,py),(vx,vy)) <- xs ]

sol1 xs = until (all (\((px,py),_) -> px >= 0 && py >= 0)) iter xs

mkVector :: [((Int,Int),(Int,Int))] -> Int -> Int -> V.Vector Char
mkVector xs maxX maxY = result
  where fill a [] = return a
        fill a (((px,py),_):ys) = do
                MV.write a (px*maxY + py) '#'
                fill a ys
        result :: V.Vector Char
        result = V.create $ do
                a <- MV.replicate ((maxX+1)*(maxY+1)) ' '
                fill a xs

printVector v maxX maxY = mapM_ printLine [0..maxY]
  where printLine y = print [ v ! (x*maxY + y) | x <- [0..maxX] ]

toStrings v maxX maxY = [ [ v ! (x*maxY + y) | x <- [0..maxX] ] | y <- [0..maxY] ]


foo s1 i = if maxX - minX < 100 && maxY - minY < 100 && hasAdj
           then do
            --printVector result maxX maxY
            mapM_ print strs
            print i
            xs <- getLine
            if length xs > 0 && head xs == 'q'
              then return ()
              else foo (iter s1) (i+1)
           else foo (iter s1) (i+1)
  where minX = minimum [ px | ((px,_),_) <- s1 ]
        maxX = maximum [ px | ((px,_),_) <- s1 ]
        minY = minimum [ py | ((_,py),_) <- s1 ]
        maxY = maximum [ py | ((_,py),_) <- s1 ]
        result = mkVector s1 maxX maxY
        strs = toStrings result maxX maxY
        hasAdj = any ("###" `isInfixOf`) strs


main = do xs <- getMyInput
          --let s1 = sol1 xs
          foo xs 0
