module Creature where

data Creature
    = Player
    | DeadPlayer
    | SmartMonster
    | GhostMonster
    | ChaseMonster
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
isMonster ChaseMonster        = True
isMonster CalmMonster         = True
isMonster FastMonster         = True
isMonster BasicMonster        = True
