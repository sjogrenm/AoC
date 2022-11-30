import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
import Data.Array
import Debug.Trace (trace, traceShow)
import qualified Data.Vector as V
--import Data.Vector ((!))
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
import Data.Tree

import Util

regexStr = do f <- readFile "20.txt"
              let [x] = lines f
              return x

type Regex = [RegexPrim]

data RegexPrim = Nil | Match String | Split [Regex]
        deriving (Eq,Show,Ord)


type P a = Parsec [Char] () a

parseRegex :: String -> Regex
parseRegex s = let Right r = parse (between (char '^') (char '$') regex) "regex" s
                in r

regex :: P Regex
regex = {-collapse `fmap`-} many regexPrim

regexPrim :: P RegexPrim
regexPrim = try parseSplit <|> (Match `fmap` many1 letter)

parseSplit :: P RegexPrim
parseSplit = Split `fmap` between (char '(') (char ')') (sepBy1 regex (char '|'))


move 'W' (x,y) = (x-1,y)
move 'E' (x,y) = (x+1,y)
move 'N' (x,y) = (x,y-1)
move 'S' (x,y) = (x,y+1)

redundant :: String -> Bool
redundant s = foldr move (0,0) s == (0,0)

--removeRedundant (Seq xs) = collapse $ map removeRedundant xs
--removeRedundant (Split xs) = 

notNil Nil = False
notNil _ = True

{-

collapse :: [Regex] -> Regex
collapse xs = case filter notNil xs of
                [] -> []
                [x] -> x
                ys -> ys
                -}

countSteps xs = sum (map countSteps' xs)
countSteps' (Match xs) | redundant xs = 0
                       | otherwise    = length xs
countSteps' (Split xs) = maximum (map countSteps xs)
countSteps' Nil = 0


--[ [p1,p2], [p3,p4] ] -> [p1,p3],[p1,p4],[p2,p3],[p2,p4]

paths :: Regex -> Tree String
--paths [] = []
--paths (x:xs) = paths' x (paths xs)
paths r = case paths2 r [] of
            [t] -> t
            ts  -> Node "" ts

paths2 :: Regex -> Forest String -> Forest String
paths2 [] f = f
paths2 (x:xs) f = paths' x (paths2 xs f)

paths' :: RegexPrim -> Forest String -> Forest String
paths' Nil f = f
paths' (Match s) f = [Node s f]
paths' (Split xs) f = concat [ paths2 x f | x <- xs ]

combine [] = [[]]
combine (xs:xss) = [ x ++ ys | ys <- combine xss, x <- xs ]


{-
dropPrefix n (Node xs f) = case splitAt n xs of
                             (_, []) -> Node "" $ map (dropPrefix (n - length xs)) f
                             (_, zs) -> Node zs f

countNodes (Node xs f) = length xs + sum (map countNodes f)
-}

sol1 = countSteps . parseRegex

--sol2 n s = countNodes (dropPrefix n ps)
--  where ps = paths $ parseRegex s

sol2 n s = walk n s --countNodes (let (r,0) = dropPrefix n (parseRegex s) in r)

main = do s <- regexStr
          print (sol1 s)
          print (sol2 1000 s)


mkSplit :: [Regex] -> RegexPrim
mkSplit xs = case nub (sort xs) of
               [] -> Nil
               [[Nil]] -> Nil
               xs -> Split xs

dropPrefix :: Int -> Regex -> (Regex, Int)
dropPrefix 0 rs = (rs, 0)
dropPrefix n (Match xs : r) = case splitAt n xs of
                                 (_, []) -> dropPrefix (n - length xs) r
                                 (_, zs) -> (Match zs : r, 0)
dropPrefix n (Nil : r) = dropPrefix n r
dropPrefix n (Split xs : r) = ([ mkSplit [ fst (dropPrefix n' (filter notNil (x'++r)))
                                         | x <- xs, not (isRedundant n x), let (x',n') = dropPrefix n x ] ], 0)
--dropPrefix n (Split xs : r) = [Split [ dropPrefix n (x++r) | x <- xs, not (isRedundant n x) ]]
dropPrefix n [] = ([], n)


isRedundant n [Match x] = n >= length x && redundant x
isRedundant _ _ = False

--simplifyRegex (Match a : Match b : rs) = simplifyRegex (Match (a++b) : rs)
--simplifyRegex (Nil:rs) = simplifyRegex rs
--simplifyRegex (r:rs) = r : simplifyRegex rs
--simplifyRegex [] = []
simplifyRegex = filter notNil

{-
countNodes :: Regex -> Int
countNodes (Match x : rs) = length x + countNodes rs
countNodes (Nil : rs) = countNodes rs
countNodes (Split xs : rs) = sum (map countNodes xs) + countNodes rs
countNodes [] = 0
-}

countNodes = Set.size . countNodes' (Set.singleton (0,0), (0,0))

countNodes' :: (Set.Set (Int,Int), (Int,Int)) -> Regex -> Set.Set (Int,Int)
countNodes' vsp (Match x : rs) = countNodes' vsp' rs
  where vsp' = foldl addRoom vsp x
--        addRoom :: Char -> (Set.Set (Int,Int), (Int,Int)) -> (Set.Set (Int,Int), (Int,Int))
        addRoom (vs,p) x = let p' = move x p in (Set.insert p' vs, p')
countNodes' vsp (Nil : rs) = countNodes' vsp rs
countNodes' vsp (Split xs : rs) = foldr1 Set.union [ countNodes' vsp (x++rs) | x <- xs ]
countNodes' (vs,p) [] = vs


--[Split [[],[Match "N",Split [[Match "ESSSSW",Split [[Match "NWSW"],[Match "SSEN"]]],[Match "WSWWN",Split [[Match "E"],[Match "WWS",Split [[Match "E"],[Match "SS"]]]]]]]]]


{-
           
            
      ..     
      ...      
        X       
        ..     
         ..        
                
-}

walk n s = Set.size $ walk' s Set.empty (Set.singleton (0,0)) [(n,(0,0))]

walk' "$" vs ini [_] = vs `Set.difference` ini
walk' ('^':xs) vs ini stack = walk' xs vs ini stack
walk' ('(':xs) vs ini (s:stack) = walk' xs vs ini (s:s:stack)
walk' (')':xs) vs ini (s:stack) = walk' xs vs ini stack
walk' ('|':xs) vs ini (_:s:stack) = walk' xs vs ini (s:s:stack)
walk' (x:xs) vs ini ((n,pos):stack) | n > 1     = walk' xs vs (Set.insert pos' ini) ((n-1,pos'):stack)
                                    | otherwise = walk' xs (Set.insert pos' vs) ini ((0,pos'):stack)
  where pos' = move x pos
