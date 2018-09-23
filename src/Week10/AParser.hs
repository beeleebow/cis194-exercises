{-# LANGUAGE InstanceSigs #-}

module Week10.AParser
  ( Parser(..)
  , satisfy
  , char
  , posInt
  , abParser
  , abParser_
  , intPair
  , intOrUppercase
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad (void)
import Data.Char (isDigit, isUpper, chr)

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

---------------------------  Exercise 1

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser (fmap (first f) . runParser p) 
  
---------------------------  Exercise 2

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (\s -> Just(a,s))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pf pa = Parser (
    \s -> do 
      (f, rest) <- runParser pf s
      (a, rest') <- runParser pa rest
      return (f a, rest')
    )

  -- long hand
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- (<*>) pf pa = Parser (\s -> case runParser pf s of
  --   Nothing -> Nothing
  --   Just (f, rest) -> case runParser pa rest of
  --     Nothing -> Nothing
  --     Just (a, rest') -> Just (f a, rest'))

---------------------------  Exercise 3

-- Unravelling the types
-- char :: Char -> Parser Char 
-- (<*>) :: Parser (Char -> Char) -> Parser Char -> Parser (Char, Char)
-- ((,) <$> char 'a') :: Parser (b -> (Char, b))
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = () <$ abParser

intPair :: Parser [Int]
intPair = (\m n -> [fromInteger m, fromInteger n]) <$> posInt <* char ' ' <*> posInt

---------------------------  Exercise 4

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) fa1 fa2 = Parser (
    \s -> case runParser fa1 s of
      Nothing -> runParser fa2 s 
      j -> j
    )

---------------------------  Exercise 5

intOrUppercase :: Parser ()
intOrUppercase = (() <$ satisfy isUpper) <|> (() <$ posInt)
