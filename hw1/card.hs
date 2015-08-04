toDigits :: Integer -> [Integer]
toDigits n
    | n == 0    = []
    | n < 0     = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10] 

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n == 0    = []
    | n < 0     = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOtherFromStart :: [Integer] -> [Integer]
doubleEveryOtherFromStart []     = []
doubleEveryOtherFromStart (x:[]) = [x]
doubleEveryOtherFromStart (x:y:zs) = x : (y*2) : doubleEveryOtherFromStart zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherFromStart (reverse xs))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (x `div` 10) + (x `mod` 10) + sumDigits xs

validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0