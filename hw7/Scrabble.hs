{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
                deriving (Eq, Ord, Num, Show)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

getScore :: Score -> Int
getScore (Score x) = x

score :: Char -> Score
score c = case c' of
            'A' -> Score 1
            'B' -> Score 3
            'C' -> Score 3
            'D' -> Score 2
            'E' -> Score 1
            'F' -> Score 4
            'G' -> Score 2
            'H' -> Score 4
            'I' -> Score 1
            'J' -> Score 8
            'K' -> Score 5
            'L' -> Score 1
            'M' -> Score 3
            'N' -> Score 1
            'O' -> Score 1
            'P' -> Score 3
            'Q' -> Score 10
            'R' -> Score 1
            'S' -> Score 1
            'T' -> Score 1
            'U' -> Score 1
            'V' -> Score 4
            'W' -> Score 4
            'X' -> Score 4
            'Y' -> Score 4
            'Z' -> Score 10
            otherwise -> Score 0
            where c' = toUpper c

scoreString :: String -> Score
scoreString = mconcat . map score
