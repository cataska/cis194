module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add expr1 expr2) = (eval expr1) + (eval expr2)
eval (Mul expr1 expr2) = (eval expr1) * (eval expr2)

evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
               Just a -> Just (eval a)
               _ -> Nothing

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  add expr1 expr2 = Add expr1 expr2
  mul expr1 expr2 = Mul expr1 expr2

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit x = x
  add expr1 expr2 = expr1 + expr2
  mul expr1 expr2 = expr1 * expr2

instance Expr Bool where
  lit x = x <= 0
  add expr1 expr2 = expr1 || expr2
  mul expr1 expr2 = expr1 && expr2

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 ((a+b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a*b) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7
