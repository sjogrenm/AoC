import Data.Array.MArray
import Data.Array.ST
import Debug.Trace (trace)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad.ST

parseInput :: IO [Int]
parseInput = do contents <- readFile "5input.txt"
                return [ read l | l <- lines contents ]


mkArray :: [Int] -> ST s (STUArray s Int Int)
mkArray xs = newListArray (0, length xs - 1) xs

{-
process' hi arr curr steps f = --do es <- getElems arr
                               --   trace (show es ++ ", curr=" ++ show curr) $
                                do i <- readArray arr curr
                                   (lo,hi) <- getBounds arr
                                   let curr' = curr + i
                                   if (curr' < lo || curr' > hi)
                                     then return steps
                                     else do writeArray arr curr (f i)
                                             process' arr curr' (steps+1) f
-}

process is f = runST $ do arr <- mkArray is
                          process' arr 0 0
  where ub = length is
        process' arr curr steps
          | curr < 0 || curr >= ub      = return steps
          | otherwise =
                   do i <- readArray arr curr
                      let curr' = curr + i
                      writeArray arr curr (f i)
                      process' arr curr' (steps+1)

sol1 = do is <- parseInput
          return $ process is (+1)

sol2 = do is <- parseInput
          return $ process is f
  where f x | x >= 3 = x - 1
        f x          = x + 1


process2 is f = runST $ V.thaw nums >>= p 0 0
  where nums = V.fromList is
        p c i v | i < 0 || i >= M.length v      = return c
                | otherwise = do
                        val <- M.read v i
                        M.write v i (f val)
                        p (c+1) (i+val) v

sol2' = do is <- parseInput
           return $ process2 is f
  where f x | x >= 3 = x - 1
        f x          = x + 1

main = do ff <- sol2'
          print ff
