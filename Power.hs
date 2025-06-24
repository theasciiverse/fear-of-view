module Power where

-- XXX: If adding a power with a longer name, check still fits in inventory
-- window when we have >= 10 charges.
data PowerType
    = Heal
    | Dazzle
    | Smoke
    | Haste
    | Teleport
    | Undo
    | Ghost
    | Foresight
    deriving (Eq, Ord, Show)

data Power = Power
    { tp         :: PowerType
    , charges    :: Int
    , maxCharges :: Int
    , overUsable :: Bool
    }

setOverUsable :: Bool -> Power -> Power
setOverUsable b pow = pow { overUsable = b }

new :: PowerType -> Bool -> Power
new tp' = Power tp' 0 0

activatable :: Power -> Bool
activatable pow = charges pow > 0 || overUsable pow

activatableTimes :: Power -> Int
activatableTimes pow
    | overUsable pow = charges pow + maxCharges pow
    | otherwise = charges pow

upgrade, recharge :: Power -> Power
upgrade pow =
    -- |Capped at 99 so will fit in inventory window
    pow { maxCharges = min 99 $ maxCharges pow + 1, charges = min 99 $ charges pow + 1 }
recharge pow = pow {charges = maxCharges pow}

deplete :: Power -> Maybe Power
deplete pow
    | charges pow > 0 = Just $ pow { charges = charges pow - 1 }
    | maxCharges pow > 1 = Just $ pow { maxCharges = maxCharges pow - 1 }
    | otherwise = Nothing
