module Golf where

import Data.List

skips :: [a] -> [[a]]
skips [] = []
skips (x : xs) = [x : xs] ++ skips xs

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = []
localMaxima (x : y : []) = []
localMaxima (x : y : z : xs)
    | y > x && y > z = y : localMaxima (y : z : xs)
    | otherwise = localMaxima (y : z : xs)

histogram :: [Integer] -> String
histogram xs = histolines ++ "\n" ++ bottom
    where
      groups = map (\x -> (head x, length x)) . group $ sort xs
      emptyGroups = map (\x -> (x, 0)) [0..9]
      maxHeight = maximum $ map snd groups
      columns = sort $ unionBy (\(x,_) (y,_) -> x == y) groups emptyGroups
      buildColumn (_,c) = reverse $ take maxHeight $ replicate c '*' ++ repeat ' '
      histolines = intercalate "\n" $ transpose $ map buildColumn columns
      bottom = "==========\n0123456789\n"
