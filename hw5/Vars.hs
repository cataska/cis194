{-# LANGUAGE FlexibleInstances #-}
module Vars where

import qualified Data.Map as M
import Parser

class HasVars a where
  var :: String -> a

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
              deriving Show

instance Expr VarExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y

instance HasVars VarExprT where
  var s = Var s

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
   lit x _ = Just x
   mul x y z = case (x z, y z) of
                 (Nothing, _) -> Nothing
                 (_, Nothing) -> Nothing
                 (Just a, Just b) ->  Just (a * b)
   add x y z = case (x z, y z) of
                 (Nothing, _) -> Nothing
                 (_, Nothing) -> Nothing
                 (Just a, Just b) ->  Just (a + b)

withVars :: [(String, Integer)]
          -> (M.Map String Integer -> Maybe Integer)
          -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
