module Rand where

import           Control.Monad.Random

import qualified Data.Set             as S

-- | Argument must be non-empty
randElemUnsafe :: [a] -> Rand StdGen a
randElemUnsafe as = (as!!) <$> getRandomR (0, length as - 1)

randElem :: [a] -> Rand StdGen (Maybe a)
randElem [] = pure Nothing
randElem as = Just <$> randElemUnsafe as

-- | Argument must be non-empty
randMemberUnsafe :: S.Set a -> Rand StdGen a
randMemberUnsafe s = (S.toList s !!) <$> getRandomR (0,S.size s - 1)

randMember :: S.Set a -> Rand StdGen (Maybe a)
randMember s | S.null s = pure Nothing
randMember s = Just <$> randMemberUnsafe s

shuffle :: [a] -> Rand StdGen [a]
shuffle = liftRand . uniformShuffleList
