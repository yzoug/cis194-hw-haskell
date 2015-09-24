---- Homework 1
-- Ex 1
toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0    = []
    | otherwise = toDigits (x `div` 10) ++ [(x `mod` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0    = []
    | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

-- Ex 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverseList (doubleFromLeft (reverseList xs)) 
    where reverseList :: [Integer] -> [Integer]
          reverseList []     = []
          reverseList (x:xs) = reverseList xs ++ [x]
          doubleFromLeft :: [Integer] -> [Integer]
          doubleFromLeft (x:[]) = x:[]
          doubleFromLeft (x:y:[]) = x:2*y:[]
          doubleFromLeft (x:y:xs) = x:2*y:(doubleFromLeft xs)

-- Ex 3
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = if x `div` 10 <= 0 then x + sumDigits xs else bigNumber x + sumDigits xs
    where bigNumber :: Integer -> Integer --returns sum digits for num>=10
          bigNumber x = sum (toDigits x)

-- Ex 4
validate :: Integer -> Bool
validate x = sumDigits(doubleEveryOther(toDigits x)) `mod` 10 == 0

-- Ex 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c = 

