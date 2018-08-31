{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Week07.JoinList
  ( JoinList(..)
  , joinListToList
  , (!!?)
  , (+++)
  , indexJ
  , dropJ
  , takeJ
  , scoreLine
  , foldJoinList
  , tag
  , main
  ) where

import Data.Monoid
  ( (<>)
  , Product(..)
  )
import Week07.Buffer (Buffer(..))
import Week07.Editor (editor, runEditor)
import Week07.Scrabble (Score(..), scoreString, getScore)
import Week07.Sized (Sized(..), Size(..), getSize)

data JoinList m a
  = Empty
  | Single m
           a
  | Append m
           (JoinList m a)
           (JoinList m a)
  deriving (Eq, Show)

joinListToList :: JoinList m a -> [a]
joinListToList Empty = []
joinListToList (Single _ a) = [a]
joinListToList (Append _ l r) = joinListToList l ++ joinListToList r

(!!?) :: Int -> [a] -> Maybe a
(!!?) n = lookup n . zip [0 ..]

--------------------------- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag Empty = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ a = a
a +++ Empty = a
as +++ bs = Append (tag as <> tag bs) as bs

--------------------------- Exercise 2

sizeOfList :: (Sized b, Monoid b) => JoinList b a -> Int
sizeOfList = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i l | i >= sizeOfList l = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Append _ l r) =
  if i >= sizeOfList l
  then indexJ (i - sizeOfList l) r
  else indexJ i l
indexJ _ (Single _ a) = Just a

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i l | sizeOfList l <= i = Empty
dropJ i s@(Single _ _)
  | i == 0 = s
  | otherwise = Empty
dropJ i (Append _ l r) =
  if i >= sizeOfList l
  then dropJ (i - sizeOfList l) r
  else dropJ i l +++ r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i l | i >= sizeOfList l = l
takeJ i s@(Single _ _)
  | i > 0 = s
  | otherwise = Empty
takeJ i (Append _ l r) =
  if i < sizeOfList l
  then takeJ i l
  else l +++ takeJ (i - sizeOfList l) r

--------------------------- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

--------------------------- Exercise 4

foldJoinList ::
     (a -> b -> b)
  -> (b -> b -> b)
  -> b
  -> JoinList m a
  -> b
foldJoinList _ _ z Empty = z
foldJoinList f _ z (Single _ a) = f a z
foldJoinList f g z (Append _ l r) = g (foldJoinList f g z l) (foldJoinList f g z r)

singleton :: String -> JoinList (Score, Size) String
singleton s = Single (scoreString s, Size 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString :: JoinList (Score, Size) String -> String
  toString = foldJoinList (++) (<>) ""

  fromString :: String -> JoinList (Score, Size) String
  fromString = fromLines . lines where
    fromLines [] = Empty
    fromLines [l] = singleton l
    fromLines ls = fromLines firstHalf +++ fromLines secondHalf where
      (firstHalf, secondHalf) = splitAt ((length ls + 1) `div` 2) ls

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = indexJ

  replaceLine ::
       Int
    -> String
    -> JoinList (Score, Size) String
    -> JoinList (Score, Size) String
  replaceLine i _ l | i >= numLines l = l
  replaceLine _ r (Single _ _) = singleton r
  replaceLine i r l = takeJ i l +++ singleton r +++ dropJ (i + 1) l

  numLines :: JoinList (Score, Size) String -> Int
  numLines = getSize . snd . tag

  value :: JoinList (Score, Size) String -> Int
  value = getScore . fst . tag

initialValue :: JoinList (Score, Size) String
initialValue = fromString $ unlines
  [ "This is the initial buffer for JointList (Score, Size) String."
  , "It should be faster."
  , "Or so the story goes..."
  ]

main :: IO ()
main =
  runEditor editor initialValue
