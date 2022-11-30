import Data.Maybe
import Data.Ord
import Data.Char
import Data.List
import Data.Array
import Debug.Trace (trace, traceShow)
import qualified Data.Vector as V
--import Data.Vector ((!))
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies
import Data.Graph as G
import Data.Bits
import Text.Parsec
import Text.Parsec.Char

import Util

data Attack = Slashing | Bludgeoning | Radiation | Fire | Cold
        deriving (Eq, Show)

data Unit = U { uSide :: String
              , uCount :: Int
              , uHP :: Int
              , uImmunities :: [Attack]
              , uWeaknesses :: [Attack]
              , uDmg :: Int
              , uAtk :: Attack
              , uInit :: Int
              }
        deriving (Eq)

instance Show Unit where
    show (U side count hp is ws dmg atk init)
      = "(" ++ side ++ ") " ++ show count ++ " units each with " ++ show hp ++ " hit points "
        ++ showWsIs is ws ++ "with an attack that does " ++ show dmg ++ " "
        ++ show atk ++ " damage at initiative " ++ show init

showWsIs [] [] = ""
showWsIs is [] = "(" ++ showIs is ++ ") "
showWsIs [] ws = "(" ++ showWs ws ++ ") "
showWsIs is ws = "(" ++ showIs is ++ "; " ++ showWs ws ++ ") "

showWs, showIs :: [Attack] -> String
showWs ws = "weak to " ++ concat (intersperse ", " $ map show ws)
showIs is = "immune to " ++ concat (intersperse ", " $ map show is)

{-
597 units each with 4458 hit points with an attack that does 73 slashing damage at initiative 6
4063 units each with 9727 hit points (weak to radiation) with an attack that does 18 radiation damage at initiative 9
2408 units each with 5825 hit points (weak to slashing; immune to fire, radiation) with an attack that does 17 slashing damage at initiative 2
5199 units each with 8624 hit points (immune to fire) with an attack that does 16 radiation damage at initiative 15
1044 units each with 4485 hit points (weak to bludgeoning) with an attack that does 41 radiation damage at initiative 3
4890 units each with 9477 hit points (immune to cold; weak to fire) with an attack that does 19 slashing damage at initiative 7
1280 units each with 10343 hit points with an attack that does 64 cold damage at initiative 19
609 units each with 6435 hit points with an attack that does 86 cold damage at initiative 17
480 units each with 2750 hit points (weak to cold) with an attack that does 57 fire damage at initiative 11
807 units each with 4560 hit points (immune to fire, slashing; weak to bludgeoning) with an attack that does 56 radiation damage at initiative 8
-}
immune = [ U "immune"  597  4458 [] [] 73 Slashing 6
         , U "immune" 4063  9727 [] [Radiation] 18 Radiation 6
         , U "immune" 2408  5825 [Fire, Radiation] [Slashing] 17 Slashing 2
         , U "immune" 5199  8624 [Fire] [] 16 Radiation 15
         , U "immune" 1044  4485 [] [Bludgeoning] 41 Radiation 3
         , U "immune" 4890  9477 [Cold] [Fire] 19 Slashing 7
         , U "immune" 1280 10343 [] [] 64 Cold 19
         , U "immune"  609  6435 [] [] 86 Cold 17
         , U "immune"  480  2750 [] [Cold] 57 Fire 11
         , U "immune"  807  4560 [Fire, Slashing] [Bludgeoning] 56 Radiation 8
         ]

{-
1237 units each with 50749 hit points (weak to radiation; immune to cold, slashing, bludgeoning) with an attack that does 70 radiation damage at initiative 12
4686 units each with 25794 hit points (immune to cold, slashing; weak to bludgeoning) with an attack that does 10 bludgeoning damage at initiative 14
1518 units each with 38219 hit points (weak to slashing, fire) with an attack that does 42 radiation damage at initiative 16
4547 units each with 21147 hit points (weak to fire; immune to radiation) with an attack that does 7 slashing damage at initiative 4
1275 units each with 54326 hit points (immune to cold) with an attack that does 65 cold damage at initiative 20
436 units each with 36859 hit points (immune to fire, cold) with an attack that does 164 fire damage at initiative 18
728 units each with 53230 hit points (weak to radiation, bludgeoning) with an attack that does 117 fire damage at initiative 5
2116 units each with 21754 hit points with an attack that does 17 bludgeoning damage at initiative 10
2445 units each with 21224 hit points (immune to cold) with an attack that does 16 cold damage at initiative 13
3814 units each with 22467 hit points (weak to bludgeoning, radiation) with an attack that does 10 cold damage at initiative 1
-}
infection = [ U "infection" 1237 50749 [Cold, Slashing, Bludgeoning] [Radiation] 70 Radiation 12
            , U "infection" 4686 25794 [Cold, Slashing] [Bludgeoning] 10 Bludgeoning 14
            , U "infection" 1518 38219 [] [Slashing, Fire] 42 Radiation 16
            , U "infection" 4547 21147 [Radiation] [Fire] 7 Slashing 4
            , U "infection" 1275 54326 [Cold] [] 65 Cold 20
            , U "infection"  436 36859 [Fire, Cold] [] 164 Fire 18
            , U "infection"  728 53230 [] [Radiation, Bludgeoning] 117 Fire 5
            , U "infection" 2116 21754 [] [] 17 Bludgeoning 10
            , U "infection" 2445 21224 [Cold] [] 16 Cold 13
            , U "infection" 3814 22467 [] [Bludgeoning, Radiation] 10 Cold 1
            ]


effPower unit = uCount unit * uDmg unit

u1 `damageTo` u2 | uSide u1 == uSide u2         = -1
                 | immune       = 0
                 | weak         = 2 * effPower u1
                 | otherwise    = effPower u1
  where weak = uAtk u1 `elem` uWeaknesses u2
        immune = uAtk u1 `elem` uImmunities u2


selectTarget u us = case [ t | t <- us, u `damageTo` t > 0 ] of
                      [] -> Nothing
                      ts -> Just $ maximumBy (comparing f) ts
  where f t = (u `damageTo` t, effPower t, uInit t)


targetSelection [] _ = []
targetSelection _ [] = []
targetSelection (u:us) ts = case selectTarget u ts of
                              Just t  -> (u,t) : targetSelection us (ts\\[t])
                              Nothing -> targetSelection us ts


iter imm inf = pairings
  where immTargets = targetSelection (sortBy (comparing effPower) imm) inf
        infTargets = targetSelection (sortBy (comparing effPower) inf) imm
        pairings = sortBy (comparing $ uInit . fst) (immTargets ++ infTargets)
