module Item where

import qualified Pos as P

data Item
    = Gem
    | ScoreTreasure
    | Junk
    | UmbrellaHandle P.Dir Int
    | CameraBoxed
    | Potion
    | MiniPotion
    | RollingOrb { orbRollDir :: P.Dir, orbJustDropped :: Bool }
    | ItemInvItem InvItem
    deriving (Eq, Ord)

data InvItem
    = Cloak
    | Orb
    | Umbrella Int
    | Balloon Int
    | Flash
    | Camera Int
    | Tent
    | Spraypaint Int
    deriving (Eq, Ord, Show, Read)

initCameraCharge, initBalloonCharges, initSpraypaintCharges, initUmbrellaCharges :: Int
initCameraCharge = 10
initBalloonCharges = 3
initSpraypaintCharges = 5
initUmbrellaCharges = 8

findableTreasures :: [Item]
findableTreasures =
    [ ItemInvItem Cloak
    , ItemInvItem Orb
    , ItemInvItem (Umbrella initUmbrellaCharges)
    , ItemInvItem (Balloon initBalloonCharges)
    , ItemInvItem Flash
    , ItemInvItem Tent
    , ItemInvItem (Spraypaint initSpraypaintCharges)
    , CameraBoxed
    ]
