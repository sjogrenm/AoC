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
import Text.Parsec
import Text.Parsec.Expr

import Util

data Expr = Expr :+: Expr | Expr :*: Expr | C Int
    deriving Show

input1 = parseInput p' "18.txt"
  where p' s = case runParser (p buildExpr1) () "18.txt" s of
                 Left e -> error (show e)
                 Right r -> r

input2 = parseInput p' "18.txt"
  where p' s = case runParser (p buildExpr2) () "18.txt" s of
                 Left e -> error (show e)
                 Right r -> r

parens :: Parsec String () a -> Parsec String () a
parens p = try $ do char '('
                    spaces
                    x <- p
                    spaces
                    char ')'
                    return x

number = do x <- many1 digit
            return (read x)

p :: (Expr -> [(Char,Expr)] -> Expr) -> Parsec String () Expr
p buildExpr =
    do e <- pPrim
       spaces
       foo <- many $ do op <- char '+' <|> char '*'
                        spaces
                        e <- pPrim
                        spaces
                        return (op,e)
       return $ buildExpr e foo
  where pPrim = try (do char '('
                        spaces
                        e <- p buildExpr
                        spaces
                        char ')'
                        return e)
                <|> (do x <- number
                        return (C x))

buildExpr1 a [] = a
buildExpr1 a (('+',e):es) = buildExpr1 (a :+: e) es
buildExpr1 a (('*',e):es) = buildExpr1 (a :*: e) es

buildExpr2 a [] = a
buildExpr2 a (('+',e):es) = buildExpr2 (a :+: e) es
buildExpr2 a (('*',e):es) = a :*: buildExpr2 e es

eval (a :+: b) = eval a + eval b
eval (a :*: b) = eval a * eval b
eval (C x) = x

sol xs = sum [ eval s | s <- xs ]

main = do i1 <- input1
          --mapM_ print i
          print (sol i1)
          i2 <- input2
          print (sol i2)
