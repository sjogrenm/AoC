import Control.DeepSeq
import Control.Monad.ST
import Data.Char
import Data.List
import Data.Ord
import Data.Array
import Debug.Trace
import Text.Parsec

parseInput = do i <- readFile "20input.txt"
                return (parseStr i)

sample = "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"

parseStr str = [ pt | xs <- lines str, let Right pt = parse parsePoint "" xs ]

type Triple = (Int,Int,Int)

data Point = Point { p :: Triple, v :: Triple, a :: Triple }
        deriving (Show, Eq, Ord)

instance NFData Point where
        rnf (Point a b c) = rnf (a,b,c)

parsePoint = do [p,v,a] <- sepBy parseTriple (char ',' >> spaces)
                return (Point p v a)

parseTriple = do letter
                 char '='
                 [a,b,c] <- between (char '<') (char '>') (sepBy num (char ','))
                 return (a,b,c)

num = do ds <- many1 (char '-' <|> digit)
         return (read ds :: Int)

plus :: Triple -> Triple -> Triple
(a,b,c) `plus` (d,e,f) = (a+d, b+e, c+f)

step :: Point -> Point
step (Point p v a) = Point p' v' a
  where v' = v `plus` a
        p' = p `plus` v'


manhattan :: Triple -> Int
manhattan (a,b,c) = abs a + abs b + abs c

sol1 pts = minimumBy f (zip pts [0..])
  where f (Point _ _ a1, _) (Point _ _ a2, _) = comparing manhattan a1 a2


stepAll :: [Point] -> [Point]
stepAll ps = map step $ force $ remDup ps

remDupBy f (a:b:cs) = case f a b of
                        EQ -> remDupBy f (a:cs)
                        _  -> a : remDupBy f (b:cs)
remDupBy _ xs = xs

remDup xs = [ g | [g] <- groupBy (eq p) $ sortBy (comparing p) xs ]

eq p x y = p x == p y

sol2 iter = do ps <- parseInput
               let foo = iterate stepAll ps !! iter
               print (length foo)
               return foo

accel (Point _ _ a) (Point _ _ b) = compare a b

foo ps 0 = return ()
foo ps n = do print (length ps)
              let ps' = stepAll ps
              foo ps' (n-1)

sample2 = concatMap parseStr [ "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>"
          ,  "p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>"
          ,  "p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>"
          ,  "p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>"
          ]

--main = sol2 10000 >> return ()

main = do ps <- parseInput
          foo ps 100
