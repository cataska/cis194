{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

import Data.List
import Data.Maybe

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n | n >= 2 = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(x, y) -> (y, x+y)) (0,1)

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show s = show $ take 20 (streamToList s)

streamToList :: Stream a -> [a]
streamToList (Cons a stm) = a : streamToList stm

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a stm) = Cons (f a) (streamMap f stm)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

nats :: Stream Integer
nats = streamFromSeed succ 0

powerOf2 :: [(Integer, Integer)]
powerOf2 = zip [0..] $ map (2^) [0..]

ruler' :: [Integer]
ruler' = map f [1..] where
  f n = fst ((reverse $ takeWhile (\(_, y) -> n `mod` y == 0) powerOf2) !! 0)

interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream (Cons x xs) ys = Cons x (interleaveStream ys xs)

startRuler n = interleaveStream (streamRepeat n) (startRuler (n+1))

ruler :: Stream Integer
ruler = startRuler 0

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate (Cons x xs) = Cons (-x) (negate xs)
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
  (*) (Cons x xs) yys@(Cons y ys) = Cons (x * y) (streamMap (*x) ys + (xs * yys))

instance Fractional (Stream Integer) where
  (/) (Cons x xs) (Cons y ys) = q where
    q = Cons (x `div` y) (streamMap (`div` y) (xs - q*ys))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - (x^2))

data Matrix = Matrix Integer Integer Integer Integer deriving Show

instance Num (Matrix) where
  (*) (Matrix a11 a12
              a21 a22)
      (Matrix b11 b12
              b21 b22) =
    Matrix (a11*b11 + a12*b21) (a11*b12 + a12*b22)
           (a21*b11 + a22*b21) (a21*b12 + a22*b22)

fibM = Matrix 1 1 1 0

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 1 = 1
fibs4 n = proj (fibM^n) where
  proj (Matrix _ x _ _) = x
