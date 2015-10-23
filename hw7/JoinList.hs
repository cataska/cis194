{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a `mappend` tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ j1 j2) = jlToList j1 ++ jlToList j2

sz :: (Sized m, Monoid m) => JoinList m a -> Int
sz = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i jl
  | i < 0 = Nothing
  | i >= sz jl = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append m j1 j2)
  | i < s1 = indexJ i j1
  | otherwise = indexJ (i - s1) j2
  where
    s1 = sz j1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl
  | i <= 0 = jl
  | i >= sz jl = Empty
dropJ _ (Single _ _) = Empty
dropJ i (Append m j1 j2)
  | i < s1 = (dropJ i j1) +++ j2
  | otherwise = dropJ (i - s1) j2
  where
    s1 = sz j1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i jl
  | i <= 0 = Empty
  | i >= sz jl = Empty
takeJ _ s@(Single _ _) = s
takeJ i (Append m j1 j2)
  | i < s1 = takeJ i j1
  | otherwise = takeJ (i - s1) j2
  where
    s1 = sz j1

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
  toString = concat . jlToList

  fromString = foldr (\l acc -> toSingle l +++ acc) Empty . lines
               where toSingle s = Single ((scoreString s), (Size 1)) s

  line = indexJ

  replaceLine n s l = takeJ (n-1) l +++ fromString s +++ dropJ n l

  numLines = getSize . snd . tag

  value = getScore . fst . tag

initBuffer :: JoinList (Score, Size) String
initBuffer = fromString . unlines $
             [ "This buffer is for notes you don't want to save, and for"
             , "evaluation of steam valve coefficients."
             , "To load a different file, type the character L followed"
             , "by the name of the file."
             ]

main = runEditor editor $ initBuffer
