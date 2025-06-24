{-# LANGUAGE LambdaCase #-}

module Inventory where

import           Data.List       ((\\))
import qualified Data.Map.Strict as M
import           Safe

import           Item

type Slot = Int
newtype Inventory = Inventory { invItems :: M.Map Slot InvItem }

empty :: Inventory
empty = Inventory M.empty

null :: Inventory -> Bool
null (Inventory inv) = M.null inv

modInvItems :: (M.Map Slot InvItem -> M.Map Slot InvItem) -> Inventory -> Inventory
modInvItems f inv = inv { invItems = f $ invItems inv }

slots :: [Slot]
slots = [1..8]

swap :: Slot -> Slot -> Inventory -> Inventory
swap n m = modInvItems . M.mapKeys $ \case
    k | k == n -> m
    k | k == m -> n
    k          -> k

add :: InvItem -> Inventory -> (Bool,Inventory)
add e (Inventory inv)
    | Just n <- headMay $ slots \\ M.keys inv = (True, Inventory $ M.insert n e inv)
    | otherwise = (False, Inventory inv)

clearAllBut :: Int -> Inventory -> Inventory
clearAllBut n = modInvItems $ M.fromList . zip [1..] . (snd <$>) . take n . M.toList
