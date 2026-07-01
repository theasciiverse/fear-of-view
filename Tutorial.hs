module Tutorial where

import           Creature
import           Item
import qualified Power    as Pow

import qualified Pos      as P

data Beat
    = Meta
    | Meta2
    | Movement P.Pos Int
    | Trapped P.Pos Int
    | Hurt Int Int
    | CollectItem
    | CollectItem2
    | CollectGem Pow.PowerType
    | CollectScore
    | SeeMonster P.Pos Creature
    | SeeItem P.Pos Item
    | SeePotion P.Pos
    | SeeScore P.Pos
    | SeeJunk P.Pos
    | SeeGem P.Pos
    | SeeExit P.WPos
    | MustBreak
    | Timer
    | TutEnd
    deriving (Eq, Ord)

data Type
    = TMeta
    | TMeta2
    | TMovement
    | TTrapped
    | TSeeMonster
    | TSeeExit
    | TSeeItem
    | TSeePotion
    | TSeeScore
    | TSeeJunk
    | TSeeGem
    | TMustBreak
    | TCollectItem
    | TCollectItem2
    | TCollectGem
    | TCollectScore
    | THurt
    | TTimer
    | TTutEnd
    deriving (Eq, Ord, Enum, Bounded, Show)

tp :: Beat -> Type
tp Meta             = TMeta
tp Meta2            = TMeta2
tp (Movement _ _)   = TMovement
tp (Trapped _ _)    = TTrapped
tp (SeeMonster _ _) = TSeeMonster
tp (SeeExit _)      = TSeeExit
tp (SeeItem _ _)    = TSeeItem
tp (SeePotion _)    = TSeePotion
tp (SeeScore _)     = TSeeScore
tp (SeeJunk _)      = TSeeJunk
tp (SeeGem _)       = TSeeGem
tp MustBreak        = TMustBreak
tp CollectItem      = TCollectItem
tp CollectItem2     = TCollectItem2
tp (CollectGem _)   = TCollectGem
tp CollectScore     = TCollectScore
tp (Hurt _ _)       = THurt
tp Timer            = TTimer
tp TutEnd           = TTutEnd

allTypes :: [Type]
allTypes = [minBound..maxBound]

text :: Beat -> String
text = text' . tp
    where
     -- Not longer than this (with scrW = 60 and appending "  [Spc/T]")
     -- "##################################################"
    text' TMeta =
        "Welcome. Press space, or T to disable game hints."
    text' TMeta2 =
        "These hints are just hints. Experiment to learn."
    text' TMovement =
        "Move using cursors / WASD / HJKL."
    text' TTrapped =
        "Trapped! You can't rest, but can walk into a wall."
    text' TSeeMonster =
        "Something you'd rather not see, still less touch."
    text' TSeeExit =
        "You found the way out of here, at last."
    text' TMustBreak =
        "You must break the seal, despite the consequences."
    text' TSeeItem =
        "Why are things always in the last place you look?"
    text' TSeePotion =
        "A drink? That would make you feel better."
    text' TSeeScore =
        "This is just what you're looking for."
    text' TSeeJunk =
        "A load of junk. Maybe you could salvage something."
    text' TSeeGem =
        "You think you see a shining gemstone."
    text' TCollectItem =
        "An item! Hit 1 then a direction to try to use it."
    text' TCollectItem2 =
        "Experiment to learn how to use each kind of item."
    text' TCollectGem =
        "The gem released a power. Press 0 to activate it."
    text' TCollectScore =
        "Enough of these, and you can get out of here!"
    text' THurt =
        "Watch where you walk."
    text' TTimer =
        "You feel the walls closing in."
    text' TTutEnd =
        "That was the last hint. You're on your own now."
