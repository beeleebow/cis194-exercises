module Week02.LogAnalysis
  ( parseMessage
  , parse
  , insert
  , build
  , inOrder
  , whatWentWrong
  , module Week02.Log
  ) where

import Week02.Log
  ( LogMessage(..)
  , MessageTree(..)
  , MessageType(..)
  , TimeStamp
  , testWhatWentWrong
  , testParse
  )

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  "E":l:t:r -> LogMessage (Error (read l)) (read t) (unwords r)
  "I":t:r -> LogMessage Info (read t) (unwords r)
  "W":t:r -> LogMessage Warning (read t) (unwords r)
  ws -> Unknown (unwords ws)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- ************************************* Fancy ******************************************
-- parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert x Leaf = Node Leaf x Leaf
insert x@(LogMessage _ t1 _) (Node lt y@(LogMessage _ t2 _) rt)
  | t1 < t2 = Node (insert x lt) y rt
  | otherwise = Node lt y (insert x rt)
insert _ _ = undefined

build :: [LogMessage] -> MessageTree
build xs =
  let
    accumulate ys tree = case ys of
      [] -> tree
      (z:zs) -> accumulate zs (insert z tree)
  in
    accumulate xs Leaf

-- ************************************* Fancy ******************************************
-- build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf x rt) = x : inOrder rt
inOrder (Node lt x rt) = inOrder lt ++ [x] ++ inOrder rt

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs =
  let
    isSevere (LogMessage (Error n) _ _) = n >= 50
    isSevere _ = False
    accumulate [] = []
    accumulate (Unknown _:ys) = accumulate ys
    accumulate (y@(LogMessage _ _ m):ys) =
      if isSevere y
      then m : accumulate ys
      else accumulate ys
  in
    accumulate (inOrder (build xs))

-- ************************************* Fancy ******************************************
-- whatWentWrong =
--   let
--     isSevere (LogMessage (Error n) _ _) = n >= 50
--     isSevere _ = False
--     getMsg (LogMessage _ _ m) = m
--   in
--     map getMsg . filter isSevere . inOrder . build
