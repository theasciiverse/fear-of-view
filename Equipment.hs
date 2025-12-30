module Equipment where

data Equipment
    = Bag
    | Charm
    | Hook
    | Key
    | Siphon
    deriving (Eq, Ord, Show, Enum, Bounded)

allEquipment :: [Equipment]
allEquipment = [minBound..maxBound]
