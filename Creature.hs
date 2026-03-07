module Creature where

data Creature
    = Player Int
    | SmartMonster
    | GhostMonster
    | CalmMonster
    | FastMonster
    | BasicMonster
    | InflatedBalloon Int
    deriving (Eq, Ord)

isMonster, isPlayer :: Creature -> Bool
isMonster (Player _)          = False
isMonster (InflatedBalloon _) = False
isMonster SmartMonster        = True
isMonster GhostMonster        = True
isMonster CalmMonster         = True
isMonster FastMonster         = True
isMonster BasicMonster        = True
isPlayer (Player _) = True
isPlayer _          = False

monsterDamage :: Creature -> Int
monsterDamage CalmMonster = 1
monsterDamage m | isMonster m = 2
monsterDamage _ = 0
