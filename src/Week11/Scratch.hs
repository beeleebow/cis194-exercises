{-# LANGUAGE InstanceSigs #-}

import Prelude (String, Int, Show, Eq, (.), const, undefined)

class Functor f where
  (<$>) :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  
  infixl 4 <*>

instance Functor ((->) r) where
  (<$>) :: (a -> b) -> ((->) r a) -> ((->) r b)
  (<$>) = (.) 

instance Applicative ((->) r) where
  pure :: a -> (r -> a)
  pure = const

  (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
  ff <*> fa = \r -> ff r (fa r)

data Employee = Employee
  { eName :: String
  , eAge :: Int
  , eNumSickDays :: Int
  } deriving (Show, Eq)

-- Employee :: String -> Int -> Int -> Employee
-- eName :: Employee -> String
-- eAge :: Employee -> Int
-- eNumSickDays :: Employee -> Int

data BigRecord = BR 
  { brName         :: String
  , brSSN          :: String
  , brSalary       :: Int
  , brAge          :: Int
  , brLicensePlate :: String
  , brNumSickDays  :: Int
  } deriving (Show, Eq)

record :: BigRecord
record = BR "Dan" "XXX-XX-XXX4" 600000000 37 "JGX-55T3" 2

-- brName :: BigRecord -> String
-- brAge :: BigRecord -> Int
-- Employee <$> brName :: BigRecord -> Int -> Employee
-- f a = getName :: Employee -> String
getEmp :: BigRecord -> Employee
getEmp = Employee <$> brName <*> brAge <*> brNumSickDays

-- (BigRecord -> Int -> Employee) -> (BigRecord -> Int) -> (BigRecord -> Employee)
-- brName :: BigRecord -> String
-- brAge :: BigRecord -> Int
-- Employee :: String -> Int -> Employee




