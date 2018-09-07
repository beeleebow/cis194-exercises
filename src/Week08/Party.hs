module Week08.Party
  ( glCons
  , moreFun
  , nextLevel
  , maxFun
  , main
  ) where

import Week08.Employee
  ( GuestList(..)
  , Employee(..)
  , Fun
  )
import Data.List (sort)
import Data.Monoid ((<>))
import Data.Tree (Tree, foldTree)

--------------------------- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ f) (GL es ft) = GL (e:es) (f + ft)

-- See src/Week08/Employee.hs to implement
-- the monoid instance for GuestList.
-- Avoids orphans.

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

--------------------------- Exercise 2

-- foldTree is defined in Data.Tree, go read it if you like
-- https://hackage.haskell.org/package/containers-0.6.0.1/docs/src/Data.Tree.html#foldTree

--------------------------- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (glCons boss guestsUnderBoss, glWithoutBoss) where
  glWithoutBoss   = (mconcat . map fst) gls
  guestsUnderBoss = (mconcat . map snd) gls

--------------------------- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . foldTree nextLevel

--------------------------- Exercise 5

printHeader :: Fun -> IO ()
printHeader f = putStrLn $ "Total fun: " ++ show f

printLines :: GuestList -> IO ()
printLines = mapM_ putStrLn . sort . map empName . glGuests

printGuestList :: GuestList -> IO ()
printGuestList gl = do
  printHeader (glFun gl)
  printLines gl

main :: IO ()
main = do
  contents <- readFile "./resources/Week08/company.txt"
  let optimalGuestList = maxFun $ read contents
  printGuestList optimalGuestList
