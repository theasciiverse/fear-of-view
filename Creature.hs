module Creature where

data Creature
    = Player Int
    | SmartMonster Bool
    | FastMonster Bool
    | CalmMonster Bool
    | GhostMonster Bool
    | BasicMonster
    | InflatedBalloon Int
    deriving (Eq, Ord)

isMonster, isPlayer :: Creature -> Bool
isMonster (Player _)          = False
isMonster (InflatedBalloon _) = False
isMonster (SmartMonster _)    = True
isMonster (GhostMonster _)    = True
isMonster (CalmMonster _)     = True
isMonster (FastMonster _)     = True
isMonster BasicMonster        = True
isPlayer (Player _) = True
isPlayer _          = False

monsterDamage :: Creature -> Int
monsterDamage (CalmMonster False) = 1
monsterDamage m | isMonster m = 2
monsterDamage _ = 0
