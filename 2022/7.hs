import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
import Data.Array
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

import Util

input = do cont <- readFile "7.txt"
           let _:ls = lines cont
           return ls


ex = ["$ ls"
     , "dir a"
     , "14848514 b.txt"
     , "8504156 c.dat"
     , "dir d"
     , "$ cd a"
     , "$ ls"
     , "dir e"
     , "29116 f"
     , "2557 g"
     , "62596 h.lst"
     , "$ cd e"
     , "$ ls"
     , "584 i"
     , "$ cd .."
     , "$ cd .."
     , "$ cd d"
     , "$ ls"
     , "4060174 j"
     , "8033020 d.log"
     , "5626152 d.ext"
     , "7214296 k"]

data DirOrFile = Dir String [DirOrFile] | File String Int
    deriving (Eq,Show)

process dirStack [] = goToRoot dirStack
process (d@(Dir curr fs1) : dirStack) (cmd:cmds)
  = case words cmd of
      ["$","ls"]      -> let (cont,cmds') = span (\(x:_) -> x /= '$') cmds
                             fs2 = map parseFile cont
                          in process (Dir curr (fs1++fs2) : dirStack) cmds'
      ["$","cd",".."] -> process (navUp (d:dirStack)) cmds
                         --let Dir parent fs2 : others = dirStack
                         --    fs2' = updateDir d fs2
                         -- in process (Dir parent fs2' : others) cmds
      ["$","cd",dir]  -> process (Dir dir [] : d : dirStack) cmds
process [] cmds = error (show (cmds))

navUp (d:Dir parent fs:others) = Dir parent (updateDir d fs) : others

goToRoot [d] = d
goToRoot ds = goToRoot (navUp ds)

parseFile xs = case words xs of
                 ["dir",dir] -> Dir dir []
                 [sz,fname] -> File fname (read sz)

updateDir (Dir s cs) (Dir s' [] : fs) | s == s'     = Dir s cs : fs
updateDir d (f:fs) = f : updateDir d fs
updateDir (Dir s _) _ = error $ "Dir " ++ s ++ " not found"


size (File name sz) = sz
size (Dir name fs) = sum $ map size fs

sizes prefix d@(Dir name fs) = (fullName, size d) : concatMap (sizes fullName) [ d | d@(Dir _ _) <- fs ]
  where fullName = prefix ++ "/" ++ name


sol1 sz = sum [ s | (_,s) <- sz, s <= 100000 ]

sol2 sz@((_,rsize):_) = head $ dropWhile (< missing) $ sort (map snd sz)
  where missing = rsize - (70000000 - 30000000)

main = do i <- input
        --   print (head i)
          let root = process [Dir "/" []] i
              sz = sizes "" root
          print (sol1 sz)
          print (sol2 sz)
