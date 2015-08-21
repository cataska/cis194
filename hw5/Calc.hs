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
