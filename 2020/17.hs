import Data.Bits
import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace, traceShow)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as Map

import Util

inputText = ["##..#.#."
            ,"#####.##"
            ,"#######."
            ,"#..#..#."
            ,"#.#...##"
            ,"..#....#"
            ,"....#..#"
            ,"..##.#.."]

exText    = [".#."
            ,"..#"
            ,"###"]

type State1 = Array (Int,Int,Int) Char
type State2 = Array (Int,Int,Int,Int) Char


initial tss = listArray ((0,0,0), (length tss-1,length tss-1,0)) (repeat '.') // [ ((x,y,0),'#') | (ts,y) <- zip tss [0..], ('#',x) <- zip ts [0..] ]

initial2 tss = listArray ((0,0,0,0), (length tss-1,length tss-1,0,0)) (repeat '.') // [ ((x,y,0,0),'#') | (ts,y) <- zip tss [0..], ('#',x) <- zip ts [0..] ]

data State = State { ranges :: [(Int,Int)], active :: Set.Set [Int] }
    deriving Show

initial3 extraDim tss = State ([(0,length tss-1), (0,length tss-1)] ++ replicate extraDim (0,0)) $
                          Set.fromList [ [x,y] ++ replicate extraDim 0 | (ts,y) <- zip tss [0..], ('#',x) <- zip ts [0..] ]

needsResize st = or $ [ or [ st ! (xmin,y,z) == '#' || st ! (xmax,y,z) == '#' | y <- [ymin..ymax], z <- [zmin..zmax] ]
                      , or [ st ! (x,ymin,z) == '#' || st ! (x,ymax,z) == '#' | x <- [xmin..xmax], z <- [zmin..zmax] ]
                      , or [ st ! (x,y,zmin) == '#' || st ! (x,y,zmax) == '#' | x <- [xmin..xmax], y <- [ymin..ymax] ]
                      ]
  where ((xmin,ymin,zmin),(xmax,ymax,zmax)) = bounds st

needsResize2 st = or $ [ or [ st ! (xmin,y,z,w) == '#' || st ! (xmax,y,z,w) == '#' | y <- [ymin..ymax], z <- [zmin..zmax], w <- [wmin..wmax] ]
                       , or [ st ! (x,ymin,z,w) == '#' || st ! (x,ymax,z,w) == '#' | x <- [xmin..xmax], z <- [zmin..zmax], w <- [wmin..wmax] ]
                       , or [ st ! (x,y,zmin,w) == '#' || st ! (x,y,zmax,w) == '#' | x <- [xmin..xmax], y <- [ymin..ymax], w <- [wmin..wmax] ]
                       , or [ st ! (x,y,z,wmin) == '#' || st ! (x,y,z,wmax) == '#' | x <- [xmin..xmax], y <- [ymin..ymax], z <- [zmin..zmax] ]
                       ]
  where ((xmin,ymin,zmin,wmin),(xmax,ymax,zmax,wmax)) = bounds st

grow st = listArray ((xmin-1,ymin-1,zmin-1),(xmax+1,ymax+1,zmax+1)) (repeat '.') // assocs st
  where ((xmin,ymin,zmin),(xmax,ymax,zmax)) = bounds st

grow2 st = listArray ((xmin-1,ymin-1,zmin-1,wmin-1),(xmax+1,ymax+1,zmax+1,wmax+1)) (repeat '.') // assocs st
  where ((xmin,ymin,zmin,wmin),(xmax,ymax,zmax,wmax)) = bounds st

iter st = st' // changes st'
  where st' = if needsResize st then grow st else st
        changes st = [ (c,fromJust jv) | (c,e) <- assocs st, let jv = toggle st c e, isJust jv ]
        toggle st c '#' | activeNeighbours st c `elem` [2,3]  = Nothing
                        | otherwise                            = Just '.'
        toggle st c '.' | activeNeighbours st c == 3          = Just '#'
                        | otherwise                            = Nothing
        activeNeighbours st (x,y,z) = length [ () | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1],
                                                    x /= x' || y /= y' || z /= z',
                                                    x' >= xmin, x' <= xmax, y' >= ymin, y' <= ymax, z' >= zmin, z' <= zmax,
                                                    st ! (x',y',z') == '#' ]
        ((xmin,ymin,zmin),(xmax,ymax,zmax)) = bounds st

iter2 st = st' // changes st'
  where st' = if needsResize2 st then grow2 st else st
        changes st = [ (c,fromJust jv) | (c,e) <- assocs st, let jv = toggle st c e, isJust jv ]
        toggle st c '#' | activeNeighbours st c `elem` [2,3]  = Nothing
                        | otherwise                            = Just '.'
        toggle st c '.' | activeNeighbours st c == 3          = Just '#'
                        | otherwise                            = Nothing
        activeNeighbours st (x,y,z,w) = length [ () | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1], w' <- [w-1..w+1],
                                                      x /= x' || y /= y' || z /= z' || w /= w',
                                                      x' >= xmin, x' <= xmax, y' >= ymin, y' <= ymax, z' >= zmin, z' <= zmax, w' >= wmin, w' <= wmax,
                                                      st ! (x',y',z',w') == '#' ]
        ((xmin,ymin,zmin,wmin),(xmax,ymax,zmax,wmax)) = bounds st

numActive st = length [ () | '#' <- elems st ]

printSt st = sequence_ [ putStrLn ("z="++show z) >> printLayer st z
                       | z <- [zmin..zmax]
                       , or [ st ! (x,y,z) == '#' | x <- [xmin..xmax], y <- [ymin..ymax] ]
                       ]
  where ((xmin,ymin,zmin),(xmax,ymax,zmax)) = bounds st

printLayer st z = sequence_ [ putStrLn [ (st ! (x,y,z)) | x <- [xmin..xmax] ]
                            | y <- [ymin..ymax]
                            ]
  where ((xmin,ymin,zmin),(xmax,ymax,zmax)) = bounds st

sol1 = numActive $ iterate iter (initial inputText) !! 6

sol2 = numActive $ iterate iter2 (initial2 inputText) !! 6

main = do --print inputText
          print sol1
          print sol2