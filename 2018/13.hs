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
import qualified Data.IntMap.Strict as Map
import Control.Parallel.Strategies

import Util

getInput = parseInput id "13.txt"

data Turn = L | S | R
        deriving (Eq, Show)

type CartState = (Char,(Int,Int),Turn)
type Map = V.Vector (V.Vector Char)

mkArrays :: [[Char]] -> (Map, [CartState])
mkArrays xs = let (vs, cs) = unzip [ (V.fromList ys', cs) | (y,ys) <- zip [0..] xs, let (ys', cs) = findCarts 0 y ys ]
               in (V.fromList vs, concat cs)
  where findCarts x y [] = ([], [])
        findCarts x y (a:as) = let (as',cs') = findCarts (x+1) y as
                                in if isCart a then (track a:as', (a,(x,y),L):cs')
                                               else (a:as', cs')

isCart c = c `elem` "<>^v"
track '<' = '-'
track '>' = '-'
track '^' = '|'
track 'v' = '|'

direction '<' = (-1, 0)
direction '>' = (1, 0)
direction '^' = (0, -1)
direction 'v' = (0, 1)

fromDir (-1, 0) = '<'
fromDir (1, 0) = '>'
fromDir (0, -1) = '^'
fromDir (0, 1) = 'v'

turn R '<' = '^'
turn R '^' = '>'
turn R '>' = 'v'
turn R 'v' = '<'

turn L '<' = 'v'
turn L 'v' = '>'
turn L '>' = '^'
turn L '^' = '<'

turn S c = c

--turnDir S dir = dir
--turnDir d dir = direction (turn d (fromDir dir))

(x,y) `plus` (a,b) = (x+a,y+b)

sortCarts xs = sortBy (comparing (fl.snd3)) xs

fl (x,y) = (y,x)

at a (x,y) = (a ! y) ! x

iter :: Map -> [CartState] -> ([(Int,Int)], [CartState])
iter a cs = f [] (sortCarts cs)
  where update c@(dir,pos,nextTurn)
          | track `elem` "-|"   = (dir, pos `plus` direction dir, nextTurn)
          | track `elem` "/\\"  = let dir' = turnCorner track dir
                                   in (dir', pos `plus` direction dir', nextTurn)
          | track == '+'        = let dir' = turn nextTurn dir
                                   in (dir', pos `plus` direction dir', nt nextTurn)
          | otherwise   = error $ "Cart " ++ show c ++ " off the track"
          where track = a `at` pos
        f moved [] = ([], moved)
        f moved (c:cs) = case (removeDup c' moved, removeDup c' cs) of
                              (Just moved', _)    -> let (a,b) = f moved' cs
                                                      in (pos:a, b)
                              (Nothing, Just cs') -> let (a,b) = f moved cs'
                                                      in (pos:a, b)
                              (Nothing, Nothing)  -> f (c':moved) cs
          where c'@(_,pos,_) = update c
        removeDup c [] = Nothing
        removeDup c (x:xs) | snd3 c == snd3 x   = Just xs
                           | otherwise          = (x:) `fmap` removeDup c xs
--        (crashes,cs') = f [] cs
--        bar = catMaybes [ crashAt (sortCarts (xs++[c]++ys)) | (xs,c,ys) <- foo ]
--        cs' :: [CartState]
--        cs' = sortCarts $ snd3 (unzip3 foo)

isLR c = c `elem` "<>"

turnCorner '/'  c = if isLR c then turn L c else turn R c
turnCorner '\\' c = if isLR c then turn R c else turn L c

nt L = S
nt S = R
nt R = L

--    /    <    turn left
--    /    >    turn left
--    /    ^    turn right
--    /    v    turn right

--    \    <    turn right

findBy f c [] = Nothing
findBy f c (x:xs) | f c == f x          = Just x
                  | otherwise           = findBy f c xs


crashAt ((_,p1,_):c@(_,p2,_):cs) | p1 == p2       = Just p1
crashAt (c:cs) = crashAt cs
crashAt []     = Nothing


removeCrash ((_,p1,_):c@(_,p2,_):cs) | p1 == p2   = removeCrash cs
removeCrash (c:cs) = c : removeCrash cs
removeCrash []     = []

sol1 a cs = sol1' cs
  where sol1' cs = case iter a cs of -- crashAt $ sortCarts $ trace (debugStr a cs) cs of
                     ([], cs') -> sol1' cs'
                     (pos:_, _) -> pos

sol2 a cs = sol2' cs
  where sol2' cs = case iter a cs of
                     (_, [(_,pos,_)]) -> pos
                     (_, cs')         -> sol2' $ traceShow cs' cs'
--                     (cs', Nothing) -> sol2' cs'
--                     (cs', Just _) -> case removeCrash $ traceShow cs' cs' of
--                                        [] -> error "oops?"
--                                        [(_,pos,_)] -> pos
--                                        cs'' -> sol2' cs''

debugStr a cs = unlines ax
  where ax = [ V.toList $ V.create (f y) | y <- [0..V.length a-1] ]
        f y = let v = a ! y
               in do v' <- V.thaw v
                     sequence_ [ do curr <- MV.read v' x
                                    if isCart curr
                                      then MV.write v' x 'X'
                                      else MV.write v' x cart
                               | (cart,(x,y'),_) <- cs,
                                 y == y' ]
                     return v'
        {-
        a' = V.create $
                do ma <- V.mapM V.thaw a
                   ma' <- V.thaw ma
                   sequence_ [ do v <- MV.read ma' y
                                  MV.write v x cart
                                  MV.write ma' y v
                             | (cart,(x,y),_) <- cs ]
                   return ma'
                   -}

--debugStr a (c@(_,pos,_):cs) = "[" ++ show c ++ " " ++ [a `at` pos] ++ "], " ++ debugStr a cs
--debugStr _ [] = ""


example = [ "/->-\\        "
          , "|   |  /----\\"
          , "| /-+--+-\\  |"
          , "| | |  | v  |"
          , "\\-+-/  \\-+--/"
          , "  \\------/   " ]


doExample = let (a, cs) = mkArrays example
             in sol1 a cs


--debugIter a cs = do putStrLn (debugStr a cs)
--                    return (iter a cs)

main = do
        xs <- getInput
        let (a, cs) = mkArrays xs
        print (sol1 a cs)
        --cs1 <- debugIter a cs
        --cs2 <- debugIter a cs1
        print (sol2 a cs)
        return ()
