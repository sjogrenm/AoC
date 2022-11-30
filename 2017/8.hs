
import Data.List
import qualified Data.Map.Strict as M

data Instr = Instr String (Int -> Int -> Int) Int String (Int -> Int -> Bool) Int

instance Show Instr where
    show (Instr reg1 id val1 reg2 cmp val2)
        = "Instr " ++ reg1 ++ " <op> " ++ show val1
          ++ " if " ++ reg2 ++ " <cmp> " ++ show val2

parseIncDec "inc" = (+)
parseIncDec "dec" = (-)

parseCmp "<" = (<)
parseCmp "<=" = (<=)
parseCmp "==" = (==)
parseCmp "!=" = (/=)
parseCmp ">=" = (>=)
parseCmp ">" = (>)

parseInput = do contents <- readFile "8input.txt"
                return [ f (words l) | l <- lines contents ]
  where f [reg1,id,val1,"if",reg2,cmp,val2]
                = Instr reg1 (parseIncDec id) (read val1) reg2 (parseCmp cmp) (read val2)


eval (m,currmax) (Instr reg1 op val1 reg2 cmp val2)
  | reg2val `cmp` val2  = (M.insert reg1 reg1val' m, max reg1val' currmax)
  | otherwise   = (m,currmax)
  where reg2val = M.findWithDefault 0 reg2 m
        reg1val = M.findWithDefault 0 reg1 m
        reg1val' = reg1val `op` val1

sol1' is = let (m,currmax) = foldl eval (M.empty,0) is
            in foldr max 0 m

sol1 = do is <- parseInput
          let s = sol1' is
          return s

sol2' is = let (m,currmax) = foldl eval (M.empty,0) is
            in currmax

sol2 = do is <- parseInput
          return (sol2' is)
