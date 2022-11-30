import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M

parseInput f = do i <- readFile "22input.txt"
                  return $ f i

sampleStr = "..#\n#..\n..."

infected str = [ (x,y) | (y,row) <- zip [yMin..] ls, (x,c) <- zip [xMin..] row, c == '#' ]
  where ls = lines str
        xSize = length (head ls)
        ySize = length ls
        xMin = -(xSize `div` 2)
        yMin = -(ySize `div` 2)

mkSet str = S.fromList (infected str)

mkMap str = M.fromList [ (pos, '#') | pos <- infected str ]

u = 0
r = 1
d = 2
l = 3

turnR dir = (dir + 1) `mod` 4
turnL dir = (dir - 1) `mod` 4
rev dir = (dir + 2) `mod` 4

move (x,y) 0 = (x, y-1)
move (x,y) 1 = (x+1, y)
move (x,y) 2 = (x, y+1)
move (x,y) 3 = (x-1, y)

step (infected, newInf, pos, dir) = (infected', newInf', pos', dir')
  where posInfected = pos `S.member` infected
        infected' = if posInfected then S.delete pos infected else S.insert pos infected
        newInf' = if posInfected then newInf else newInf + 1
        dir' = if posInfected then turnR dir else turnL dir
        pos' = move pos dir'

sol1 n = do m <- parseInput mkSet
            let xs = iterate step (m, 0, (0,0), u)
            return (xs !! n)

infect '.' = 'W'
infect 'W' = '#'
infect '#' = 'F'
infect 'F' = '.'

step2 (infected, newInf, pos, dir) = (infected', newInf', pos', dir')
  where posState = fromMaybe '.' (M.lookup pos infected)
        newState = infect posState
        infected' = case newState of
                      '.' -> M.delete pos infected
                      _   -> M.insert pos newState infected
        newInf' = if newState == '#' then newInf + 1 else newInf
        dir' = case posState of
                 '.' -> turnL dir
                 'W' -> dir
                 '#' -> turnR dir
                 'F' -> rev dir
        pos' = move pos dir'


sol2 n = do m <- parseInput mkMap
            let xs = iterate step2 (m, 0, (0,0), u)
            return (xs !! n)

main = do (_, inf, _, _) <- sol2 10000000
          print inf
