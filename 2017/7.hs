import Data.List
import Data.Graph
import Data.Tree
import Data.Maybe
import Data.Ord

--data Foo = Foo String Int [String]
--        deriving (Eq,Show)

parseFoo :: [String] -> (Int, String, [String])
parseFoo (x:('(':y):zs) = (w, x, zs')
  where zs' = case zs of
                ("->":ws) -> map (takeWhile (/=',')) ws
                [] -> []
        w = read (takeWhile (/=')') y)

parseInput = do contents <- readFile "7input.txt"
                return [ parseFoo (words l) | l <- lines contents ]



sol1 = do xs <- parseInput
          let (g,v2n,k2v) = graphFromEdges xs
              vs = topSort g
              (_,n,_) = v2n (head vs)
          print n

freq s = [ (length x, head x) | x <- group (sort s) ]


getTotWeight (Node (_,tw) _) = tw

foo xs = sortBy (comparing fst) 
                [ (length x, head x) | x <- groupBy eq (sortBy (comparing getTotWeight) xs) ]
  where eq x y = comparing getTotWeight x y == EQ

totWeight :: Tree Int -> Tree (Int,Int)
totWeight (Node w cs) = Node (w, w + sum [ tw | Node (_,tw) _ <- cs' ]) cs'
  where cs' = map totWeight cs

wrongbad xs = case foo xs of
                ((1,x):(_,y):_) -> Just (x,y)
                _               -> Nothing


{-
Broken case is a node where all its children are ok but it's different
from its siblings
-}


argh (Node (w,tw) cs) r
  = case wrongbad cs of
      Just (n@(Node (x,tx) _), Node (_,ty) _) -> argh n (x+ty-tx)
      Nothing    -> r


okTree (Node n cs) = Node (n, length (nub tws) <= 1 && and oks) cs'
  where cs' = map okTree cs
        (tws,oks) = unzip [ (tw,o) | Node ((_,tw),o) _ <- cs' ]


sol2 = do xs <- parseInput
          let (g,v2n,k2v) = graphFromEdges xs
              vs = topSort g
              [t] = dfs g [head vs]
              t' = fmap (\v -> let (w,n,_) = v2n v in w) t
              tt = totWeight t'
          print (argh tt 0)
