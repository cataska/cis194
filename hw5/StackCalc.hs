{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module StackCalc where

import Parser
import StackVM

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr Program where
  lit a = [PushI a]
  add x y = x ++ y ++ [Add]
  mul x y = x ++ y ++ [Mul]

compile :: String -> Maybe Program
compile str = parseExp lit add mul str
