{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude (Int, String, (+), (-), undefined, (.), Show, const)

----------------- Values and Types ---------------- 

-- _values_ have _types_ 
-- anything with a type _is_ a value (so functions are values too)
-- not everything is a value though, remember how :t Maybe doesn't work?

----------------- Type constructors vs Data Constructors ----------------- 

-- Data constructors are functions that produce _values_ (and possibly take values as arguments)
-- Type constructors are 'functions' that produce _types_ (and possibly take types as arguments) 

data Maybe a = Nothing | Just a deriving Show
-- Nothing and Just are _data_ constructors. They construct values of type Maybe a
-- for some concrete a.
-- Maybe is a _type_ constructor. Given a type, it constructs a 'concrete' type 

data List a = Empty | Cons a (List a) deriving Show
-- Empty and Cons are _data_ constructors
-- List is a _type_ constructor 

data Either a b = Left a | Right b deriving Show
-- Left and Right are _data_ constructors
-- Either is a type constructor

--------------------------------------------------------------------------




























------------ What's the Kind? -------------- 

-- What's the kind of Int?
-- What's the kind of Maybe Int?
-- What's the kind of List Int? 
-- What's the kind of Either?
-- What's the kind of Either Int?
-- What's the kind of (->)?
-- What's the kind of (->) Int?        
-- What's the kind of (->) Int String?

---------------------------------------------





















------------ Okay, back to mapping things...  -------------- 

-- How can we map over a Maybe?
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = undefined

-- How can we map over a List?
mapList :: (a -> b) -> List a -> List b
mapList = undefined

-- How can we map over an Int?
mapInt :: (a -> b) -> Int -> Int
mapInt = undefined

-- How can we express the general pattern? A type class!
-- We'll call our general pattern fmap
-- We get a 'replace' or (<$) for free
-- Note the kind inference :)
 
-------------------------------------------------------------

------------ Let's write some Functor instances -------------- 

-- Maybe?

-- List?

-- Either?

-- Functions? :o

-- are these the only instances? Let's write another Maybe instance

-------------------------------------------------------------


























------------ Time for some laws -------------- 


-- "Law is order, and good law is good order." - Aristotle

-- What might be useful guarentees for functors?






























-- where F is a functor:
-- 1) fmap id F = F
-- 2) fmap (f . g) F = fmap f (fmap g F)

-- Back of the envelope proof...
-- Functor Laws for the valid Maybe instance
-- Functor laws for the silly Maybe instance
-- Functor Laws for the valid Maybe instance
-- Functor laws for the silly Maybe instance

