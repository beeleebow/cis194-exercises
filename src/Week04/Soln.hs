module Week04.Soln
  ( fun1
  , fun2
  , fun1'
  , fun2'
  , Tree(..)
  , foldTree
  , showTree
  , printTree
  , xor
  , map'
  , myFoldl
  , cartProd
  , sieveSundaram
  ) where

import qualified Data.Bits as Bits
import Data.List((\\))

---------------------------  Exercise 1

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr ((*) . subtract 2) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =
  sum .
  filter even .
  takeWhile (>1) .
  iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

---------------------------  Exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node _ lt x rt) = Node newH lt' x rt'
  where
    (lt', rt') =
      if height lt <= height rt
      then (insert a lt, rt)
      else (lt, insert a rt)
    newH = 1 + max (height lt') (height rt')

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

showTree :: Show a => Tree a -> String
showTree Leaf = ""
showTree n@(Node s _ _ _) = go s n
  where
  go _ Leaf = ""
  go i (Node h l c r) = go (i-1) l ++
    replicate (4*fromIntegral i) ' ' ++ show c ++ "-" ++ show h ++ "\n" ++ go (i-1) r

-- will print a tree in ghci, root node will be the rightmost in the printed structure
-- nodes will be printed as [value]-[height]
printTree :: Show a => Tree a -> IO ()
printTree t = putStrLn $ showTree t

---------------------------  Exercise 3

xor :: [Bool] -> Bool
xor = foldl Bits.xor False
--xor' = not . even . foldl (\a n -> a + (if n then 1 else 0)) 0

-- impl using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- impl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f = foldr (flip f)

---------------------------  Exercise 4
-- See: https://en.wikipedia.org/wiki/Sieve_of_Sundaram
-- cartProd provided for you to use in your solution

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map doublePlusOne ([1..n] \\ toRemove)
  where
    fromPair (i,j) = i + j + 2*i*j
    doublePlusOne = (+1) . (*2)
    pairs = cartProd [1..n] [1..n]
    toRemove = map fromPair $ filter (\(i,j) -> i <= j && fromPair (i, j) <= n) pairs
