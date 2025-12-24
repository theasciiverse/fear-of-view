{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pos where

import           Data.Hashable
import           GHC.Generics

import           Group

-- | y increases upwards
data Pos = Pos {x :: Int, y :: Int} deriving (Eq, Ord, Show, Read, Generic)
instance Hashable Pos
instance Semigroup Pos where
    Pos x1 y1 <> Pos x2 y2 = Pos (x1+x2) (y1+y2)
instance Monoid Pos where
    mempty = Pos 0 0

instance Grp Pos where
    neg (Pos x' y') = Pos (-x') (-y')
    zero = Pos 0 0

-- | manhatten metric
sqDist :: Pos -> Pos -> Int
sqDist p p' = let Pos x' y' = p +^ neg p' in abs x' + abs y'

-- | euclidean metric, squared
distSquared :: Pos -> Pos -> Int
distSquared p p' = let Pos x' y' = p +^ neg p' in x'*x' + y'*y'

-- up or right
data WPos = WPos {pos :: Pos, up :: Bool} deriving (Eq, Ord)

instance Action Pos WPos where
    p +^ WPos p' up' = WPos (p <> p') up'

data Dir = DUp | DRight | DDown | DLeft deriving (Eq, Ord, Read, Show)
dirs :: [Dir]
dirs = [DUp, DRight, DDown, DLeft]

dirPos :: Dir -> Pos
dirPos DUp    = Pos 0 1
dirPos DRight = Pos 1 0
dirPos DDown  = Pos 0 (-1)
dirPos DLeft  = Pos (-1) 0

posDir :: Pos -> Maybe Dir
posDir (Pos 0 1)    = Just DUp
posDir (Pos 1 0)    = Just DRight
posDir (Pos 0 (-1)) = Just DDown
posDir (Pos (-1) 0) = Just DLeft
posDir _            = Nothing

negDir, flipDirV, flipDirH, flipDirDiag :: Dir -> Dir
negDir = flipDirH . flipDirV
flipDirV DUp   = DDown
flipDirV DDown = DUp
flipDirV d     = d
flipDirH DRight = DLeft
flipDirH DLeft  = DRight
flipDirH d      = d
flipDirDiag DUp    = DRight
flipDirDiag DRight = DUp
flipDirDiag DLeft  = DDown
flipDirDiag DDown  = DLeft

wposInDir :: Pos -> Dir -> WPos
wposInDir p dir =
    let p' = dirPos dir +^ p
    in case dir of
        DUp    -> WPos p True
        DRight -> WPos p False
        DDown  -> WPos p' True
        DLeft  -> WPos p' False

posInDir :: WPos -> Dir -> Pos
posInDir (WPos p up') d
    | (up' && d == DUp) || (not up' && d == DRight) = p +^ dirPos d
    | otherwise = p

adjPoss :: WPos -> [Pos]
adjPoss (WPos p up') = [p, p +^ dirPos (if up' then DUp else DRight)]

dirsTowardsZero :: Pos -> [[Dir]]
dirsTowardsZero (Pos x' y')
    | x' < 0 = (flipDirH <$>) <$> dirsTowardsZero (Pos (-x') y')
    | y' < 0 = (flipDirV <$>) <$> dirsTowardsZero (Pos x' (-y'))
    | x' < y' = (flipDirDiag <$>) <$> dirsTowardsZero (Pos y' x')
    | x' == y' = [[DDown,DLeft],[DUp,DRight]]
    | x' > y' = [[DLeft]] <> (if y'>0 then [[DDown],[DUp]] else [[DDown,DUp]]) <> [[DRight]]
    | otherwise = [[]]

exitDir :: WPos -> Dir
exitDir (WPos (Pos _ y') True)
    | y' <= 0 = DDown
    | otherwise = DUp
exitDir (WPos (Pos x' _) False)
    | x' <= 0 = DLeft
    | otherwise = DRight
