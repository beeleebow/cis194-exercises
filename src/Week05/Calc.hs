{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Week05.Calc
  ( eval
  , evalStr
  , Expr(..)
  , MinMax(..)
  , Mod7(..)
  , compile
  , HasVars(..)
  , VarExprT(..)
  ) where

import Week05.ExprT
import Week05.Parser
import qualified Week05.StackVM as SVM
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

--------------------------------------------------- Exercise 1

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

--------------------------------------------------- Exercise 2

evalStr :: String -> Maybe Integer
evalStr = (<$>) eval . parseExp Lit Add Mul

--------------------------------------------------- Exercise 3

-- This is our type class :)
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- now write an instance for our ExprT type
instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

--------------------------------------------------- Exercise 4
-- Write instances for Integer, Bool, MinMax, and Mod7

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 i) (Mod7 j) = lit (i + j)
  mul (Mod7 i) (Mod7 j) = lit (i * j)

instance Expr SVM.Program where
  lit i = [SVM.PushI i]
  add p1 p2 = p1 ++ p2 ++ [SVM.Add]
  mul p1 p2 = p1 ++ p2 ++ [SVM.Mul]

compile :: String -> SVM.Program
compile s = fromMaybe [] (parseExp lit add mul s)

--------------------------------------------------- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = LitWithVar Integer
  | AddWithVar VarExprT VarExprT
  | MulWithVar VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = LitWithVar
  add = AddWithVar
  mul = MulWithVar

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit i _ = Just i
  add f g m = do
    x <- f m
    y <- g m
    return (x + y)
    -- OR
    -- f m >>= \x ->
    --   g m >>= \y ->
    --     return (x + y)
    -- OR
    -- case f m of
    -- Nothing -> Nothing
    -- Just x -> case g m of
    --   Nothing -> Nothing
    --   Just y -> Just (x + y)
    -- OR
    -- (+) <$> f m <*> g m
  mul f g m = do
    x <- f m
    y <- g m
    return (x * y)
    -- OR
    -- f m >>= \x ->
    --   g m >>= \y ->
    --     return (x * y)
    -- OR
    -- Nothing -> Nothing
    -- case f m of
    -- Just x -> case g m of
    --   Nothing -> Nothing
    --   Just y -> Just (x * y)
    -- OR
    -- (*) <$> f m <*> g m


-- >>= then a return ===> fmap
