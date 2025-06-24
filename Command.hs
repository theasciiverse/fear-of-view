module Command where

import qualified Inventory as I
import qualified Pos       as P

data Command
    = Dir P.Dir
    | UseInv I.Slot
    | UsePower
    | Accept
    | SkipTutorial
    | DebugAddPower | DebugAddJunk | DebugAddItems | DebugExit
    | ToggleAscii
    | Refresh | Redraw | Suspend | Clear
    | Quit | ForceQuit
    deriving (Eq, Ord, Show, Read)
