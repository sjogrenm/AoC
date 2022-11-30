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
import Data.Bits

import Util

input1 = readFile "16.txt"

fromString xs = concatMap decode xs

r xs = [ read [x] | x <- xs ]

decode :: Char -> [Int]
decode '0' = r "0000"
decode '1' = r "0001"
decode '2' = r "0010"
decode '3' = r "0011"
decode '4' = r "0100"
decode '5' = r "0101"
decode '6' = r "0110"
decode '7' = r "0111"
decode '8' = r "1000"
decode '9' = r "1001"
decode 'A' = r "1010"
decode 'B' = r "1011"
decode 'C' = r "1100"
decode 'D' = r "1101"
decode 'E' = r "1110"
decode 'F' = r "1111"
decode _ = []

fromBits bs = fb bs 0
  where fb [] acc = acc
        fb (b:bs) acc = fb bs (2 * acc + b)

data Packet = P Int PBody
    deriving (Eq,Show)

data PBody = PLit Int | POp Int [Packet]
    deriving (Eq,Show)

parsePacket (v1:v2:v3:t1:t2:t3:rest) = (P (fromBits [v1,v2,v3]) body, rest')
  where (body,rest') = case fromBits [t1,t2,t3] of
                         4  -> parseLiteral rest
                         pt -> parseOp pt rest

parseLiteral xs = (PLit (fromBits num), rest)
  where (num, rest) = parseNum xs

parseNum (1:rest) = (take 4 rest ++ num, rest')
  where (num, rest') = parseNum (drop 4 rest)
parseNum (0:rest) = splitAt 4 rest

parseOp pt (0:rest) = (POp pt ps, rest')
  where (xs,ys) = splitAt 15 rest
        subLen = fromBits xs
        (zs,rest') = splitAt subLen ys
        (ps,[]) = parsePackets zs
parseOp pt (1:rest) = (POp pt ps, rest')
  where (xs,ys) = splitAt 11 rest
        count = fromBits xs
        (ps, rest') = parsePacketsN count ys

parsePackets [] = ([], [])
parsePackets xs = (p:ps, rest')
  where (p, rest) = parsePacket xs
        (ps, rest') = parsePackets rest

parsePacketsN 0 xs = ([], xs)
parsePacketsN n xs = (p:ps, rest')
  where (p, rest) = parsePacket xs
        (ps, rest') = parsePacketsN (n-1) rest

sol1 is = sum $ versions p
  where (p,_) = parsePacket (fromString is)

versions (P v body) = v : case body of
                            POp _ ps -> concatMap versions ps
                            _        -> []

sol2 is = eval p
  where (p,_) = parsePacket (fromString is)

eval (P _ (PLit num)) = num
eval (P _ (POp op ps))
  = case op of
      0 -> sum (map eval ps)
      1 -> product (map eval ps)
      2 -> minimum (map eval ps)
      3 -> maximum (map eval ps)
      5 -> if eval (ps !! 0) > eval (ps !! 1) then 1 else 0
      6 -> if eval (ps !! 0) < eval (ps !! 1) then 1 else 0
      7 -> if eval (ps !! 0) == eval (ps !! 1) then 1 else 0

main = do is <- input1
          --print is
          print (sol1 is)
          print (sol2 is)