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

newtype Hand = H String
    deriving Show

cmpHand a b = case compare at bt of
                EQ -> cmpHand' a b
                r  -> r
  where at = handType a
        bt = handType b

cmpHand' [] [] = EQ
cmpHand' (a:as) (b:bs) = case cmpCard a b of
                           EQ -> cmpHand' as bs
                           r  -> r

cmpCard 'A' 'A' = EQ
cmpCard 'A' _   = GT
cmpCard 'K' 'A' = LT
cmpCard 'K' 'K' = EQ
cmpCard 'K' _   = GT
cmpCard 'Q' 'A' = LT
cmpCard 'Q' 'K' = LT
cmpCard 'Q' 'Q' = EQ
cmpCard 'Q' _   = GT
cmpCard 'J' b | b `elem` "AKQ"  = LT
cmpCard 'J' 'J' = EQ
cmpCard 'J' _   = GT
cmpCard 'T' b | b `elem` "AKQJ" = LT
cmpCard 'T' 'T' = EQ
cmpCard 'T' _   = GT
cmpCard a b | isDigit a && not (isDigit b)   = LT
cmpCard a b | isDigit a && isDigit b         = compare (read [a] :: Int) (read [b] :: Int)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
    deriving (Eq, Ord, Show)

handType xs = case group (sort xs) of
                [x]                                                         -> FiveOfAKind
                [x,y] | length x == 4 || length y == 4                      -> FourOfAKind
                      | otherwise                                           -> FullHouse
                [x,y,z] | length x == 3 || length y == 3 || length z == 3   -> ThreeOfAKind
                        | otherwise                                         -> TwoPair
                [x,y,z,w]                                                   -> OnePair
                _                                                           -> HighCard

handType2 xs = maximum $ map handType (expand xs)

expand ('J':xs) = [ w:ys | w <- "AKQT98765432", ys <- expand xs ]
expand (x:xs) = map (x:) (expand xs)
expand [] = [[]]

instance Eq Hand where
    H a == H b      = cmpHand a b == EQ

instance Ord Hand where
    compare (H a) (H b) = cmpHand a b


cmpHand2 (H a) (H b)
  = case compare at bt of
        EQ -> cmpHand2' a b
        r  -> r
  where at = handType2 a
        bt = handType2 b

cmpHand2' [] [] = EQ
cmpHand2' (a:as) (b:bs) = case cmpCard2 a b of
                           EQ -> cmpHand2' as bs
                           r  -> r

cmpCard2 'A' 'A' = EQ
cmpCard2 'A' _   = GT
cmpCard2 'K' 'A' = LT
cmpCard2 'K' 'K' = EQ
cmpCard2 'K' _   = GT
cmpCard2 'Q' 'A' = LT
cmpCard2 'Q' 'K' = LT
cmpCard2 'Q' 'Q' = EQ
cmpCard2 'Q' _   = GT
cmpCard2 'T' b | b `elem` "AKQ" = LT
cmpCard2 'T' 'T' = EQ
cmpCard2 'T' _   = GT
cmpCard2 'J' 'J' = EQ
cmpCard2 'J' _   = LT
cmpCard2 _ 'J'   = GT
cmpCard2 a b | isDigit a && not (isDigit b)   = LT
cmpCard2 a b | isDigit a && isDigit b         = compare (read [a] :: Int) (read [b] :: Int)


ex = [(H "32T32K", 765), (H "T55J5", 684), (H "KK677", 28), (H "KTJJT", 220), (H "QQQJA", 483)]

input = parseInput f "7.txt"
  where f s = let [x,y] = words s in (H x, read y :: Int)

sol1 i = sum [ bid * rank | ((_, bid), rank) <- zip (sort i) [1..] ]

sol2 i = sum [ bid * rank | ((_, bid), rank) <- zip (sortBy p i) [1..] ]
  where p (h1,_) (h2,_) = cmpHand2 h1 h2

main = do
        i <- input
        -- print i
        print (sol1 i)
        print (sol2 i)
        return ()


