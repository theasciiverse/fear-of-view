module Tutorial where

import           Creature
import           Item

import qualified Pos      as P

data Beat
    = Meta
    | Meta2
    | Movement P.Pos
    | Trapped P.Pos
    | Hurt Int Int
    | CollectItem
    | CollectItem2
    | CollectGem
    | CollectScore
    | SeeMonster P.Pos Creature
    | SeeItem P.Pos Item
    | SeePotion P.Pos
    | SeeMiniPotion P.Pos
    | SeeScore P.Pos
    | SeeJunk P.Pos
    | SeeGem P.Pos
    | SeeExit P.WPos
    | Timer
    | SecondRound
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
    | TSeeMiniPotion
    | TSeeScore
    | TSeeJunk
    | TSeeGem
    | TCollectItem
    | TCollectItem2
    | TCollectGem
    | TCollectScore
    | THurt
    | TTimer
    | TSecondRound
    | TTutEnd
    deriving (Eq, Ord, Enum, Bounded, Show)

tp :: Beat -> Type
tp Meta              = TMeta
tp Meta2             = TMeta2
tp (Movement _)      = TMovement
tp (Trapped _)       = TTrapped
tp (SeeMonster _ _)  = TSeeMonster
tp (SeeExit _)       = TSeeExit
tp (SeeItem _ _)     = TSeeItem
tp (SeePotion _)     = TSeePotion
tp (SeeMiniPotion _) = TSeeMiniPotion
tp (SeeScore _)      = TSeeScore
tp (SeeJunk _)       = TSeeJunk
tp (SeeGem _)        = TSeeGem
tp CollectItem       = TCollectItem
tp CollectItem2      = TCollectItem2
tp CollectGem        = TCollectGem
tp CollectScore      = TCollectScore
tp (Hurt _ _)        = THurt
tp Timer             = TTimer
tp SecondRound       = TSecondRound
tp TutEnd            = TTutEnd

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
        "That's something you'd rather not see..."
    text' TSeeExit =
        "You found the way out of here, at last."
    text' TSeeItem =
        "Why are things always in the last place you look?"
    text' TSeePotion =
        "That looks very refreshing."
    text' TSeeMiniPotion =
        "That looks quite refreshing."
    text' TSeeScore =
        "This is just what you're looking for."
    text' TSeeJunk =
        "A load of junk. Maybe you could salvage something."
    text' TSeeGem =
        "That looks powerful."
    text' TCollectItem =
        "An item! Hit 1 then a direction to try to use it;"
    text' TCollectItem2 =
        "experiment to learn how to use each kind of item."
    text' TCollectGem =
        "Press 0 to activate the power here."
    text' TCollectScore =
        "Enough of these, and you can get out of here!"
    text' THurt =
        "That hurt! Watch where you walk."
    text' TTimer =
        "It gets more dangerous here the longer you stay."
    text' TSecondRound =
        "Levels get harder based on which way you leave."
    text' TTutEnd =
        "That was the last hint. You're on your own now."
