import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Array.Unboxed

type Chunk = String

type Rule = (Chunk, Chunk)

parseInput = do i <- readFile "21input.txt"
                return $ mkMap $ parseStr i

parseStr s = [ parseRule x | x <- lines s ]

parseRule x = let [a,b] = splitOn " => " x
               in (a,b)

mkMap :: [Rule] -> M.Map Chunk Chunk
mkMap = M.fromList


sampleRules = mkMap $ parseStr "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"


type Grid = UArray (Int,Int) Char

initial :: Grid
initial = listArray ((0,0), (2,2)) (concat $ splitOn "/" ".#./..#/###")

chunkAt :: Grid -> (Int,Int) -> Int -> Chunk
chunkAt grid (x,y) sz = concat $ intersperse "/" [ [ grid ! (y+j,x+i) | i <- [0..sz-1] ]
                                                 | j <- [0..sz-1]
                                                 ]

chunksToGrid :: [[Chunk]] -> Grid
chunksToGrid cs = array ((0,0), (sz-1,sz-1))
                    [ ((y+j,x+i), b)
                    | (cr,y) <- zip cs [0,cSize..],
                      (c,x) <- zip cr [0,cSize..],
                      (a,j) <- zip (splitOn "/" c) [0..],
                      (b,i) <- zip a [0..]
                    ]
  where cSize = length (splitOn "/" (head $ head cs))
        sz = cSize * length cs

chunksToGrid2 cs = -- array ((0,0), (sz-1,sz-1))
                    [ ((y+j,x+i), b)
                    | (cr,y) <- zip cs [0,cSize..],
                      (c,x) <- zip cr [0,cSize..],
                      (a,j) <- zip (splitOn "/" c) [0..],
                      (b,i) <- zip a [0..]
                    ]
  where cSize = length (splitOn "/" (head $ head cs))
        sz = cSize * length cs

gridToChunks :: Grid -> Int -> [[Chunk]]
gridToChunks grid sz = [ [ chunkAt grid (x,y) sz | x <- [0,sz..gs] ]
                       | y <- [0,sz..gs]
                       ]
  where gs = gSize grid - 1

gSize grid = 1 + (snd . snd . bounds $ grid)

step :: M.Map Chunk Chunk -> Grid -> Grid
step rules grid = chunksToGrid [ map (apply rules) cs | cs <- chunks ]
  where chunks = gridToChunks grid (if gSize grid `mod` 2 == 0 then 2 else 3)

apply rules chunk = head $ catMaybes [ M.lookup c rules | c <- variations chunk ]

variations c = c : [flipChunkH c, flipChunkV c] ++ variations (rotChunkR c)

flipChunkH chunk = concat $ intersperse "/" [ reverse row | row <- splitOn "/" chunk ]

flipChunkV chunk = concat $ intersperse "/" (reverse $ splitOn "/" chunk)

rotChunkR chunk = concat $ intersperse "/" $ map reverse $ transpose (splitOn "/" chunk)

countPixels :: Grid -> Int
countPixels grid = length [ 1 | '#' <- elems grid ]

sol rules n = iterate (step rules) initial !! n

sol1 = do rules <- parseInput
          let g = sol rules 5
          printGrid g
          return (countPixels g)

sol2 = do rules <- parseInput
          let g = sol rules 18
          return (countPixels g)

printGrid :: Grid -> IO ()
printGrid grid = mapM_ putStrLn [ [ grid ! (y,x) | x <- [0..gSize grid-1] ] | y <- [0..gSize grid-1] ]

printChunk :: Chunk -> IO ()
printChunk c = mapM_ putStrLn (splitOn "/" c)

main = sol2 >>= print
