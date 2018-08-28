module Week01.CreditCardValidator
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  ) where

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

toDigits :: Integer -> [Integer]
toDigits n = reverseList (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft (x:y:zs) = x : 2*y : doubleEveryOtherFromLeft zs
doubleEveryOtherFromLeft [x] = [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverseList (doubleEveryOtherFromLeft (reverseList xs))

sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sum . toDigits) 0

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
