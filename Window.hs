module Window where

data Window
    = MainWin
    | StatusWin
    | BoardWin
    | InvWin
    | EquipWin
    | LevelInfoWin
    | MessageWin
    | TutorialWin
    deriving (Eq,Ord,Enum,Bounded)
allWindows :: [Window]
allWindows = [minBound..maxBound]
