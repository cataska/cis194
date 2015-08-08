module Golf where

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

-- histogram :: [Integer] -> String
-- histogram [] = "==========\n" ++ "0123456789\n"
