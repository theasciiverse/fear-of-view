module Equipment where

data Equipment
    = Bag
    | Charm
    | GrabHand
    | Key
    | Siphon
    deriving (Eq, Ord, Show, Enum, Bounded)

allEquipment :: [Equipment]
allEquipment = [minBound..maxBound]

equipmentStr :: Equipment -> String
equipmentStr GrabHand = "Grab hand"
equipmentStr e        = show e
