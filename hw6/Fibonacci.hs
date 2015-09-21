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
ruler = startRuler 0
