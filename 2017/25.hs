import Debug.Trace
import Data.Char
import Data.Ord
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Array
import qualified Data.Set as S
import qualified Data.Map.Strict as M

-- Tape xs ys represents  reverse xs ++ ys  with the cursor at the head of ys

-- Tape [1,0,1] [1,1,0]
data Tape = Tape [Int] [Int]
        deriving (Show)
                   --  ^- cursor

moveLeft (Tape (x:xs) ys) = Tape xs (x:ys)
moveLeft (Tape []     ys) = Tape [] (0:ys)

moveRight (Tape xs (y:ys)) = Tape (y:xs) ys
moveRight (Tape xs []    ) = Tape (0:xs) []

setVal v (Tape xs (_:ys)) = Tape xs (v:ys)
setVal v (Tape xs []    ) = Tape xs [v]

getVal (Tape _ (y:_)) = y
getVal (Tape _ []   ) = 0

diagCount = 12302209

stateA tape = case getVal tape of
                0 -> (moveRight (setVal 1 tape), 'B')
                1 -> (moveLeft  (setVal 0 tape), 'D')

stateB tape = case getVal tape of
                0 -> (moveRight (setVal 1 tape), 'C')
                1 -> (moveRight (setVal 0 tape), 'F')

stateC tape = case getVal tape of
                0 -> (moveLeft  (setVal 1 tape), 'C')
                1 -> (moveLeft  (setVal 1 tape), 'A')

stateD tape = case getVal tape of
                0 -> (moveLeft  (setVal 0 tape), 'E')
                1 -> (moveRight (setVal 1 tape), 'A')

stateE tape = case getVal tape of
                0 -> (moveLeft  (setVal 1 tape), 'A')
                1 -> (moveRight (setVal 0 tape), 'B')

stateF tape = case getVal tape of
                0 -> (moveRight (setVal 0 tape), 'C')
                1 -> (moveRight (setVal 0 tape), 'E')

stateFuns = listArray ('A', 'F') [stateA, stateB, stateC, stateD, stateE, stateF]

sol1' 0 (Tape xs ys, _) = sum xs + sum ys
sol1' n (tape, s) = sol1' (n-1) ((stateFuns ! s) tape)

sol1 = sol1' diagCount (Tape [] [], 'A')
