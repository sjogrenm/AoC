module Foo where

import Data.List
import Data.Char
import Data.Graph.Inductive.Query.BFS (esp)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Tree
import Debug.Trace
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map ((!))
import Data.Bits (shiftL)

data FloorState = FloorState Int [[Thing Int]]
        deriving (Show,Eq,Ord)

{-
instance Eq FloorState where
        FloorState e1 fs1 == FloorState e2 fs2  = e1 == e2 && normalize fs1 == normalize fs2

instance Ord FloorState where
        FloorState e1 fs1 <= FloorState e2 fs2 | e1 < e2   = True
                                               | e1 > e2   = False
                                               | otherwise = normalize fs1 <= normalize fs2
-}
data Thing a = Gen a | MChip a
        deriving (Eq, Ord, Show)

--instance Show a => Show (Thing a) where
--    show (Gen s) = show s ++ "G"
--    show (MChip s) = show s ++ "M"

mkFS floors = FloorState 0 $ normalize floors

teststate = mkFS [[MChip "H", MChip "L"], [Gen "H"], [Gen "L"], []]
initstate = mkFS [[Gen "Pr", MChip "Pr"], [Gen "Co", Gen "Cu", Gen "Ru", Gen "Pl"], [MChip "Co", MChip "Cu", MChip "Ru", MChip "Pl"], []]
initstate2 = mkFS [[Gen "Pr", MChip "Pr", Gen "El", MChip "El", Gen "Di", MChip "Di"], [Gen "Co", Gen "Cu", Gen "Ru", Gen "Pl"], [MChip "Co", MChip "Cu", MChip "Ru", MChip "Pl"], []]


FloorState e1 fs1 `equiv` FloorState e2 fs2 = e1 == e2 && normalize fs1 == normalize fs2

normalize fs = normalize' Map.empty fs
normalize' m [] = []
normalize' m (f:fs) = let (m', f') = normFloor m (sort f) in f' : normalize' m' fs
normFloor m [] = (m, [])
normFloor m (x:xs) = case x of
                       Gen   y -> foo Gen y
                       MChip y -> foo MChip y
  where foo c y = let (i, m') = lookupOrInsert y (Map.size m) m
                      (m'', xs') = normFloor m' xs
                   in (m'', c i : xs')


hashfun (FloorState e fs) = hashfloors fs * 4 + e
hashfloors [] = 0
hashfloors (f:fs) = hashfloors fs `shiftL` 14 + hashfloor f
hashfloor f = 0


validMoves s@(FloorState e floors) | null (floors !! e) = error (show s)
validMoves (FloorState e floors) = nub [ FloorState i (move t e i floors)
                                       | t <- moves currFloor, canRemove t currFloor, (f,i) <- adjacentFloors, canAdd t f ]
  where currFloor = floors !! e
        adjacentFloors = [ (f,i) | (f,i) <- zip floors [0..], i `elem` [e-1,e+1] ]
        move [] _ _ _ = undefined
        move xs from to floors = normalize [ if i == from then floor \\ xs else if i == to then xs++floor else floor | (floor,i) <- zip floors [0..] ]

moves :: [a] -> [[a]]
moves xs = map (:[]) xs ++ [ [a,b] | (a,i) <- zip xs [1..], b <- drop i xs ]


canRemove stuff floor = okFloor (floor \\ stuff)
canAdd stuff floor = okFloor (stuff++floor)


generators floor = [ s | Gen s <- floor ]
mchips floor = [ s | MChip s <- floor ]

okFloor floor = null gs || all (`elem` gs) mc
  where gs = generators floor
        mc = mchips floor


hasGens [] = False
hasGens (Gen _:_) = True
hasGens (_:fs) = hasGens fs

finalState (FloorState _ floors) = all null (init floors)

solve1 visited count state | finalState state         = count
solve1 visited count state = minimum' [ solve1 (s':visited) (count+1) s' | s' <- validMoves state, s' `notElem` visited ]

minimum' :: [Int] -> Int
minimum' [] = maxBound
minimum' xs = minimum xs

solveM :: Int -> FloorState -> State (Map.Map FloorState Int) [Int]
solveM count s | finalState s = return [count]
solveM count s = do visited <- get
               --     trace (show (length visited) ++ "\n") $ do
                    case Map.lookup s visited of
                      Just c | count >= c -> return []
                      otherwise           -> do
                        modify (Map.insert s count)
                        counts <- sequence [ solveM (count+1) s' | s' <- validMoves s ]
                        return $ concat counts

solve s = evalState (solveM 0 s) Map.empty

graphM :: FloorState -> State (Map.Map FloorState Node) [(FloorState, [FloorState])]
graphM s = do visited <- get
              if s `Map.member` visited
                then return []
                else do
                  --trace (show $ Map.size visited) $ do
                  let node = Map.size visited
                      visited' = Map.insert s node visited
                      ss = validMoves s
                  put visited'
                  gs <- mapM graphM ss
                  return ((s,ss) : concat gs)

graphM2 :: Maybe FloorState -> FloorState -> State (Map.Map FloorState Node, Gr FloorState ()) ()
graphM2 Nothing s =
  do n <- getNode s
     let ss = validMoves s
     mapM_ (graphM2 (Just s)) ss
graphM2 (Just o) s =
  do (visited, _) <- get
     if s `Map.member` visited
       then addEdge o s
       else do
         addEdge o s
         let ss = validMoves s
         mapM_ (graphM2 (Just s)) ss

getNode :: FloorState -> State (Map.Map FloorState Node, Gr FloorState ()) Node
getNode s = do (visited, g) <- get
               let suggested_node = Map.size visited
                   (node, visited') = lookupOrInsert s suggested_node visited
               put (visited', if node == suggested_node then insNode (node,s) g else g)
               return node

addEdge :: FloorState -> FloorState -> State (Map.Map FloorState Node, Gr FloorState ()) ()
addEdge f t = do fn <- getNode f
                 tn <- getNode t
         --        trace ("Edge from " ++ show fn ++ " to " ++ show tn) $ do
                 modify $ \(v, gr) -> (v, insEdge (fn, tn, ()) gr)

unlessHasVisited :: FloorState -> State (Map.Map FloorState Node, a) () -> State (Map.Map FloorState Node, a) ()
unlessHasVisited s m =
   do (visited, _) <- get
      if s `Map.member` visited
        then return ()
        else m

lookupOrInsert k v m = case Map.insertLookupWithKey (\_ _ o -> o) k v m of
                         (Nothing, m') -> (v, m')
                         (Just v', m') -> (v', m')

--graph :: FloorState -> Gr FloorState ()
graph s = trace (show $ Map.size visited) $
  esp iNode fNode g
  where (nodes, visited) = runState (graphM s) Map.empty
        lNodes = [ (visited ! n, n) | (n,_) <- nodes ]
        g :: Gr FloorState ()
        g = mkGraph lNodes [ (visited ! n, visited ! n', ()) | (n,ns) <- nodes, n' <- ns ]
        iNode = 0
        fNode = head [ n | (n, s) <- lNodes, finalState s ]


graph2 s = trace (show $ Map.size visited) $ esp iNode fNode g
  where (visited, g) = execState (graphM2 Nothing s) (Map.empty, empty)
        iNode = 0
        fNode = head [ n | (n, s) <- labNodes g, finalState s ]

--g s = graphFromEdges' (graph s)

solveM2 :: Int -> FloorState -> State (Map.Map FloorState Int) [Int]
solveM2 count s | finalState s = return [count]
solveM2 count s = do visited <- get
                     let ss = validMoves s
                         (visited', ss') = fudge visited ss (count+1)
                     put visited'
                     counts <- mapM (solveM2 (count+1)) ss'
                     return $ concat counts

fudge :: Map.Map FloorState Int -> [FloorState] -> Int -> (Map.Map FloorState Int, [FloorState])
fudge map []     _     = (map, [])
fudge map (s:ss) count = (map'', s'++ss')
  where map' = Map.alter f s map
        f Nothing = Just count
        f (Just c) = Just (min c count)
        s' = case Map.lookup s map of
               Just c | c <= count      -> []
               otherwise                -> [s]
        (map'',ss') = fudge map' ss count
