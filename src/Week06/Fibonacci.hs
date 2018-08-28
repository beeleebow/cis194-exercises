{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Week06.Fibonacci
  ( fib
  , fibs1
  , fibs2
  , fibs2'
  , fibs2''
  , Stream(..)
  , streamToList
  , streamRepeat
  , streamMap
  , streamFromSeed
  , nats
  , ruler
  , ruler'
  , ruler''
  , fibs3
  , fib4'
  , fib4''
  , fib4
  , cartProd
  ) where

import Data.List

---------------------------  Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

--------------------------- Exercise 2

fibs2' :: [Integer]
fibs2' = 0 : 1 : zipWith (+) fibs2' (tail fibs2')

fibs2'' :: [Integer]
fibs2'' = rollingAdd 0 1
  where
    rollingAdd :: Integer -> Integer -> [Integer]
    rollingAdd a b = a : rollingAdd b (a + b)

fibs2''' :: [Integer]
fibs2''' = [0,1] ++ unfoldr (\(n, m) -> Just (n+m, (m, n+m))) (0,1)

fibs2 :: [Integer]
fibs2 = fibs2'''

--------------------------- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

--------------------------- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed u a = Cons a (streamFromSeed u (u a))

--------------------------- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- let's just brute force this
ruler' :: Stream Integer
ruler' = streamMap nthRulerValue (streamFromSeed (+1) 1)
  where
    maxPossibleExponent = floor . logBase 2 . fromInteger
    nthRulerValue n = go n (maxPossibleExponent n)
    go n e
      | n `mod` (2 ^ e) == 0 = e
      | otherwise = go n (e-1)

-- careful with the stack...
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a bs) other = Cons a (interleaveStreams other bs)

-- sneaky stream interleaving soln
ruler'' :: Stream Integer
ruler'' = go 0
  where go n = interleaveStreams (streamRepeat n) (go (n + 1))

-- now with 100% more folding
foldStream :: (a -> b -> b) -> Stream a -> b
foldStream f (Cons a s) = f a (foldStream f s)

ruler''' :: Stream Integer
ruler''' = foldStream (interleaveStreams . streamRepeat) nats

ruler :: Stream Integer
ruler = ruler'''

--------------------------- Exercise 6

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

-- just follow your nose...
instance Num (Stream Integer) where
  fromInteger i = Cons i (streamRepeat 0)
  negate = streamMap negate
  (+) (Cons a0 as) (Cons b0 bs) = Cons (a0 + b0) (as + bs)
  (*) (Cons a0 as) b@(Cons b0 bs) = Cons (a0 * b0) (streamMap (a0 *) bs + as * b)

-- oh baby!
instance Fractional (Stream Integer) where
  a@(Cons a0 as) / b@(Cons b0 bs) = Cons (a0 `div` b0) (streamMap (`div` b0) (as - a / b * bs))

-- Sure it is. Whatever you say.
fibs3 :: Stream Integer
fibs3 = x / (1 - x - (x^ 2))

--------------------------- Exercise 7

-- solution 1
data Matrix2x2 = Matrix2x2 (Integer, Integer) (Integer, Integer) deriving (Show)

instance Num Matrix2x2 where
  Matrix2x2 (a1,b1) (c1,d1) * Matrix2x2 (a2, b2) (c2, d2) =
    Matrix2x2 (a1*a2+b1*c2, a1*b2+b1*d2) (c1*a2+d1*c2, c1*b2+d1*d2)

fib4' :: Integer -> Integer
fib4' n
  | n == 0 = 0
  | otherwise = firstRowSecondCol matrixToN
  where
    matrixToN = Matrix2x2 (1,1) (1,0) ^ n
    firstRowSecondCol (Matrix2x2 (_, fn) (_, _)) = fn

-- solution 2
newtype Matrix = Matrix [[Integer]] deriving (Show)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd zs ys = [(z,y) | z <- zs, y <- ys]

instance Num Matrix where
  Matrix as * Matrix bs = Matrix [[ sum $ zipWith (*) row col | col <- transpose bs ] | row <- as ]
  -- Or without a comphrension, unfoldr is still confusing to me...
  -- Matrix as * Matrix bs = Matrix (split $ map calculate combinations)
  --   where
  --     combinations = cartProd as (transpose bs)
  --     calculate (row, col) = sum $ zipWith (*) row col
  --     split = takeWhile (not . null) . unfoldr (Just . splitAt (length as))

fib4'' :: Integer -> Integer
fib4'' n
  | n == 0 = 0
  | otherwise = firstRowSecondCol matrixToN
  where
    matrixToN = Matrix [[1,1], [1,0]] ^ n
    firstRowSecondCol (Matrix ((_:fn:_):_)) = fn
    firstRowSecondCol _ = error "whats a noob to do? I guess I could have made my matrix explicitly 2x2 :("

fib4 :: Integer -> Integer
fib4 = fib4'

