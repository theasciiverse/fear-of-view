module RollFrom where

import           Control.Monad.Random
import           Data.List
import           Safe

data RollFrom a = RollFrom { sides :: Int, vals :: [a] }

empty :: (Eq a, Ord a) => RollFrom a
empty = RollFrom 0 []

modVals :: (Eq a, Ord a) => ([a] -> [a]) -> RollFrom a -> RollFrom a
modVals f from = from { vals = f $ vals from }
modSides :: (Int -> Int) -> RollFrom a -> RollFrom a
modSides f from = from { sides = f $ sides from }
setSides :: Int -> RollFrom a -> RollFrom a
setSides = modSides . const

roll :: (Eq a, Ord a) => RollFrom a -> Rand StdGen (Maybe a)
roll from = do
    n <- getRandomR (0, sides from - 1)
    pure $ vals from `atMay` n

add :: (Eq a, Ord a) => a -> RollFrom a -> RollFrom a
add a = modVals $ sort . (a :)

swap :: (Eq a, Ord a) => a -> a -> RollFrom a -> RollFrom a
swap a a' = add a' . modVals (delete a)
