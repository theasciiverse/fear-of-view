module Creature where

data Creature
    = Player
    | DeadPlayer
    | SmartMonster
    | GhostMonster
    | CalmMonster
    | FastMonster
    | BasicMonster
    | InflatedBalloon Int
    deriving (Eq, Ord)

isMonster :: Creature -> Bool
isMonster Player              = False
isMonster DeadPlayer          = False
isMonster (InflatedBalloon _) = False
isMonster SmartMonster        = True
isMonster GhostMonster        = True
isMonster CalmMonster         = True
isMonster FastMonster         = True
isMonster BasicMonster        = True
