{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Week12.Risk where

import Control.Monad.Random
import Control.Arrow ((&&&))
import Data.List (reverse, sort)

------------------------------------------------------------
-- Die values
newtype DieValue = DV
  { unDV :: Int
  } deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk
type Army = Int

data Battlefield = Battlefield
  { attackers :: Army
  , defenders :: Army
  } deriving (Show, Eq)

------------------------------------------------------------
-- Exercise 1
-- This is just installing the monad random module, you don't
-- have to worry about this due to the magic of nix

------------------------------------------------------------
-- Exercise 2

fightingArmySizes :: Battlefield -> (Army, Army)
fightingArmySizes (Battlefield as ds) = (min 3 (as - 1), min 2 ds)

genNDieRolls :: Int -> Rand StdGen [DieValue]
genNDieRolls n = replicateM n die

toPair :: Battlefield -> (Army, Army)
toPair = attackers &&& defenders

gameOver :: Battlefield -> Bool
gameOver (Battlefield as ds) = as < 2 || ds == 0

fight :: Battlefield -> ([DieValue], [DieValue]) -> Battlefield
fight bf (aRolls, dRolls) = uncurry Battlefield outcome
  where
    outcome = foldr fightPair (toPair bf) zipped
    sortDesc = reverse . sort
    zipped = zip (sortDesc aRolls) (sortDesc dRolls)
    fightPair p acc
      | uncurry (>) p = (fst acc, snd acc - 1)
      | otherwise = (fst acc - 1, snd acc)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf
  | gameOver bf = return bf
  | otherwise = do
      let (numAttackers, numDefenders) = fightingArmySizes bf
      attackerRolls <- genNDieRolls numAttackers
      defenderRolls <- genNDieRolls numDefenders
      return (fight bf (attackerRolls, defenderRolls))

------------------------------------------------------------
-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = battle bf >>= continueIfPossible
  where
    continueIfPossible bf'
      | gameOver bf' = return bf'
      | otherwise = invade bf'

------------------------------------------------------------
-- Exercise 4

noDefendersLeft :: Battlefield -> Bool
noDefendersLeft = (== 0) . defenders

successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM 5000 (invade bf) >>= \outcomes ->
  return (fromIntegral (length $ filter noDefendersLeft outcomes) / 5000)

------------------------------------------------------------
-- Exercise 5

-- Anyone know probability theory :p

exactSuccessProb :: Battlefield -> Double
exactSuccessProb = error "Week12.Risk#exactSuccessProb not implemented"
