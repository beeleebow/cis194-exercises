{-# LANGUAGE InstanceSigs #-}
import Data.Monoid((<>))

-- some tree type
data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Node Empty x Empty

tree :: Tree Integer
tree = Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)

-- remember this?
treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeSum :: Tree Integer -> Integer
treeSum Empty     = 0
treeSum (Node l x r)  = x + treeSum l + treeSum r

-- So looks like we need:

--The answer in the Empty case
--How to combine the recursive calls
--A tree

-- this is the generalisation from the notes
treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty        = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

-- now let's use it
treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> 1 + l + r)

treeSum' :: Tree Integer -> Integer
treeSum' = undefined

-- or just write an instance... if we can
instance Foldable Tree where
  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  -- foldr _ z Empty = z
  -- foldr f z (Node l x r) = foldr f (f x (foldr f z r)) l

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Empty = mempty
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

-- expressions can be folded to
data ExprT a = Lit Integer
  | Add ExprT ExprT
  | Mul ExprT ExprT deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit i)     = f i
exprTFold f g h (Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2)
exprTFold f g h (Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)

eval2 :: ExprT -> Integer
eval2 = exprTFold id (+) (*)

-- Is there a foldable instance for ExprT?


