import Data.Char (chr, ord)
import Data.Maybe
import Data.Ord
import Data.List
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import Debug.Trace (trace, traceShow)
import Control.Monad.ST
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Tree

import Util
import qualified IntCode
--import qualified IntCodeM

input1 :: IO [Int]
input1 = do [ls] <- lines `fmap` readFile "23.txt"
            return $ map read $ splitOn ',' ls


iter ps = (state, Map.fromListWith (++) $ concat [ triples o | o <- outs ])
  where (outs,state) = unzip [ (output,(mem2,pos2,rb2))
                             | ((mem1,pos1,rb1),inp1) <- ps,
                               let state = IntCode.runIO3 mem1 pos1 rb1 inp1
                                   IntCode.AwaitingInput mem2 pos2 rb2 output = state ]

sol1' ps = case Map.lookup 255 m of
               Just (_:y:_) -> y
               _              -> sol1' (zip state (map2inputs m))
  where (state, m) = iter ps

sol1 p = sol1' [ ((p,0,0),[i,-1]) | i <- [0..49] ]

map2inputs m = [ fromMaybe [-1] $ Map.lookup i m | i <- [0..49] ]

last2 [x,y] = [x,y]
last2 (_:xs) = last2 xs

--sol2 p = sol2' [ ((p,0,0),[i,-1]) | i <- [0..49] ] False Nothing

triples [] = []
triples (a:b:c:zs) = (a,[b,c]) : triples zs


packets [] = []
packets (a:b:c:zs) = (a,c) : packets zs


sol2' state q otherState natPkg
  | qEmpty q
        = case state of
            (mem,pos,rb):ps ->
              let IntCode.AwaitingInput mem2 pos2 rb2 output = IntCode.runIO3 mem pos rb [-1]
                  pks = triples output
                  q' = foldl (flip qPut) q pks
               in sol2' ps q' ((mem2,pos2,rb2):otherState) natPkg
            [] ->
              let q' = qPut (0, fromJust natPkg) q
               in last (fromJust natPkg) : sol2' (reverse otherState) q' [] natPkg
  | otherwise
        = case qGet q of
            ((255,yv), q') -> sol2' state q' otherState (Just yv)
            ((i,yv), q')   ->
              let allPrograms = reverse otherState ++ state
                  (ps1, (mem,pos,rb):ps2) = splitAt i allPrograms
                  IntCode.AwaitingInput mem2 pos2 rb2 output = IntCode.runIO3 mem pos rb yv
                  pks = triples output
                  q'' = foldl (flip qPut) q' pks
                  allPrograms' = ps1 ++ ((mem2,pos2,rb2):ps2)
               in sol2' allPrograms' q'' [] natPkg

sol2 p = head [ a | (a:_:_) <- gs ]
  where ips = [ (mem,pos,rb) | i <- [0..49],
                               let IntCode.AwaitingInput mem pos rb [] = IntCode.runIO3 p 0 0 [i] ]
        gs = group $ sol2' ips mkQueue [] Nothing


main = do i <- input1
          let p = UV.fromList (i ++ replicate 100 0)
          --print $ sol1 p
          print $ sol2 p

tr x = trace ("<"++show x++">") x
