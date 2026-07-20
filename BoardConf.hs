{-# LANGUAGE TupleSections #-}

module BoardConf where

import           Control.Monad.Random
import qualified Data.Map.Strict      as M
import           Data.Maybe

import           Creature
import           Rand
import           Wall

import qualified Pos                  as P
import qualified RollFrom             as RF

data BoardConf = BoardConf
    { level        :: Int
    , creatureRoll :: RF.RollFrom Creature
    , wallRoll     :: RF.RollFrom Wall
    }
emptyBoardConf :: BoardConf
emptyBoardConf = BoardConf 0 RF.empty RF.empty

modRolls :: (RF.RollFrom Creature -> RF.RollFrom Creature)
    -> (RF.RollFrom Wall -> RF.RollFrom Wall)
    -> BoardConf -> BoardConf
modRolls f f' bc = bc
    { creatureRoll = f $ creatureRoll bc
    , wallRoll = f' $ wallRoll bc
    }

-- XXX: ugly manual polymorphism ahead
data Diffable = DiffableCreature Creature | DiffableWall Wall
data RollFromDiff a
    = Add a
    | Swap a a

type BoardConfDiffs = M.Map P.Dir [RollFromDiff Diffable]
apply :: BoardConfDiffs -> P.Dir -> BoardConf -> BoardConf
apply bcd d = flip (foldr apply') (fromMaybe [] $ bcd M.!? d) where
    apply' (Add (DiffableCreature c)) = modC $ applyRFD (Add c)
    apply' (Swap (DiffableCreature c) (DiffableCreature c')) = modC $ applyRFD (Swap c c')
    apply' (Add (DiffableWall w)) = modW $ applyRFD (Add w)
    apply' (Swap (DiffableWall w) (DiffableWall w')) = modW $ applyRFD (Swap w w')
    apply' _ = id
    applyRFD :: (Eq a, Ord a) => RollFromDiff a -> RF.RollFrom a -> RF.RollFrom a
    applyRFD (Add a)     = RF.add a
    applyRFD (Swap a a') = RF.swap a a'
    modC f bc = bc { creatureRoll = f $ creatureRoll bc }
    modW f bc = bc { wallRoll = f $ wallRoll bc }

genDiffs :: BoardConf -> Rand StdGen BoardConfDiffs
genDiffs bc = (M.fromList <$>) . forM P.dirs $ \d -> (d,) <$> genDiffDir d where
    genDiffDir d    = sequence [ doCreature $ dirCTp d, doWall ]
    dirCTp P.DUp    = SmartMonster
    dirCTp P.DDown  = CalmMonster
    dirCTp P.DLeft  = GhostMonster
    dirCTp P.DRight = FastMonster
    doCreature :: (Bool -> Creature) -> Rand StdGen (RollFromDiff Diffable)
    doCreature cTp = randElemUnsafe $
        [ Add . DiffableCreature $ cTp False ] <>
        [ Swap (DiffableCreature $ cTp False) (DiffableCreature $ cTp True)
        | (2 <=) . length . filter (== cTp False) $ RF.vals (creatureRoll bc)
        ]
    doWall = join $ randElemUnsafe [ addWall, swapWall ]
    addWall = pure . Add . DiffableWall $ if level bc == 1 then Hedge else BasicWall
    swapWall = do
        swapOut <- randElem . filter upgradableWall $ RF.vals (wallRoll bc)
        case swapOut of
            Nothing -> addWall
            Just c  -> Swap (DiffableWall c) . DiffableWall <$> upgradeWall c
    wallUpgrades Hedge = [ThickHedge]
    wallUpgrades BasicWall
        | level bc == 2 = [Pillar]
        | otherwise = [Window]
    wallUpgrades Window = [BrokenWindow]
    wallUpgrades _ = []
    upgradableWall = not . null . wallUpgrades
    upgradeWall = randElemUnsafe . wallUpgrades
