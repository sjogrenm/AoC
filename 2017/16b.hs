import Control.Monad.ST
import Control.Monad
import Data.List.Split
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

parseInput = do i <- readFile "16input.txt"
                return $ parse i

data Instr = Spin Int | Exchg Int Int | Partner Char Char
        deriving Show

parse i = map f $ splitOn "," (concat $ lines i)
  where f ('s':xs) = Spin (read xs)
        f ('x':xs) = let [a,b] = splitOn "/" xs
                      in Exchg (read a) (read b)
        f ('p':a:'/':b:[]) = Partner a b

startPos = V.fromList ['a'..'p']

--eval :: V.Vector Char -> Instr -> ST s (V.Vector Char)
eval pos (Spin n) = do let (xs,ys) = V.splitAt (V.length pos - n) pos
                       return (ys `mplus` xs)
eval pos (Exchg i j) = do a <- V.indexM pos i
                          b <- V.indexM pos j
                          mpos <- V.thaw pos
                          M.write mpos i b
                          M.write mpos j a
                          V.freeze mpos
eval pos (Partner a b) = let (xs,ys') = V.span (`notElem` [a,b]) pos
                             (y,ys) = V.splitAt 1 ys'
                             (zs,ws') = V.span (`notElem` [a,b]) ys
                             (w,ws) = V.splitAt 1 ws'
                          in return $ mconcat [xs, w, zs, y, ws]

sol1' pos [] = return pos
sol1' pos (i:is) = do pos' <- eval pos i
                      sol1' pos' is

--sol1'' is = do pos <- V.thaw startPos
--               pos' <- sol1' pos is
--               V.freeze pos'

sol2' pos is 0 = return pos
sol2' pos is n = do pos' <- sol1' pos is
                    sol2' pos' is (n-1)

sol1 = do is <- parseInput
          return $ runST (sol1' startPos is)

sol2 = do is <- parseInput
          return $ runST (sol2' startPos is (1000*1000*1000))

main = do p <- sol2
          print p
