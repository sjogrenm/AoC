import Control.Monad.ST
import Data.Char
import Data.List
import Data.List.Split
import Data.Array
import Debug.Trace

parseInput = do i <- readFile "19input.txt"
                return $ parse $ lines i

parse ls = array ((0,0), (length (head ls) - 1, length ls - 1)) xs
  where xs = [ ((x,y), c) | (y,l) <- zip [0..] ls, (x,c) <- zip [0..] l ]

arrayFromList xs = listArray (0, length xs - 1) xs
i `withinBounds` a = let (lb,ub) = bounds a
                      in i >= lb && i < ub

sampleStr = [ "     |          "
            , "     |  +--+    "
            , "     A  |  C    "
            , " F---|----E|--+ "
            , "     |  |  |  D "
            , "     +B-+  +--+ " ]

sample = parse sampleStr

data Dir = North | South | West | East
        deriving (Eq,Show)

nextPos North (x,y) = (x,y-1)
nextPos South (x,y) = (x,y+1)
nextPos West (x,y) = (x-1,y)
nextPos East (x,y) = (x+1,y)

validDir North = [West,East]
validDir South = [West,East]
validDir West = [North,South]
validDir East = [North,South]

travel arr p@(x,y) dir str steps = 
  case c of
    ' ' -> (reverse str, steps)
    '|' -> travel arr (nextPos dir p) dir str (steps+1)
    '-' -> travel arr (nextPos dir p) dir str (steps+1)
    '+' -> travel arr turnedPos turnedDir str (steps+1)
    _ | isAlpha c -> travel arr (nextPos dir p) dir (c:str) (steps+1)
  where c = arr ! p
        (turnedPos, turnedDir) = head [ (p',d')
                                      | d' <- validDir dir
                                      , let p' = nextPos d' p
                                      , arr ! p' /= ' '
                                      ]

solSample = travel sample (5,0) South "" 0

findStart arr = head [ (x,0) | x <- [0..], arr ! (x,0) == '|' ]

sol1 = do i <- parseInput
          let pos = findStart i
          print (travel i pos South "" 0)
