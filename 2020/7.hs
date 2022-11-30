import Data.Maybe
import Data.Ord
import Data.List
import Data.Array
import Data.Char
import Debug.Trace (trace)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map

import Util

data Rule = String `Contains` [(Int,String)]
    deriving Show

type InvRules = Map.Map String [String]

input1 = parseInput (f . words) "7.txt"
  where f (t1:t2:_:_:"no":_) = (t1++" "++t2) `Contains` []
        f (t1:t2:_:_:rest) = (t1++" "++t2) `Contains` [ (read num, b1++" "++b2) | (num:b1:b2:_) <- splitOnBy s rest ]
        s xs = xs `elem` ["bag,", "bags,"]

inv :: [Rule] -> InvRules
inv [] = Map.empty
inv (b `Contains` bs:xs) = let m = inv xs
                            in foldr (\v -> Map.insertWith (++) v [b]) m (map snd bs)

sol1 m = Set.size $ sol1' "shiny gold" m Set.empty

sol1' b m curr = Set.foldr (\k c -> sol1' k m (Set.insert k c)) curr candidates
  where candidates = case Map.lookup b m of
                       Just bs -> Set.fromList bs `Set.difference` curr
                       Nothing -> Set.empty

sol2 rs = sol2' (Map.fromList [ (k,v) | k `Contains` v <- rs ]) "shiny gold" - 1

sol2' m b = 1 + sum [ n * sol2' m k | (n,k) <- fromMaybe [] (Map.lookup b m) ]

main = do i <- input1
          --print i
          let i' = inv i
          --print i'
          print (sol1 i')
          print (sol2 i)
