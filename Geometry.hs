module Geometry where

import qualified Data.Map.Strict as M

import           Window

import qualified Board           as B
import qualified Inventory       as I

-- Terminal size
scrW,scrH :: Int
scrW = 60
scrH = 20

data WinDim = WinDim
    { windimx :: Int
    , windimy :: Int
    , windimw :: Int
    , windimh :: Int
    }

geometry :: M.Map Window WinDim
geometry = M.fromList
    [ (StatusWin, WinDim 0 0 scrW 2)
    , (BoardWin, WinDim 1 3 (bdcW+1) bdcH)
    , (InvWin, WinDim (1+bdcW+3) 3 20 (3 + length I.slots))
    , (EquipWin, WinDim (1+bdcW+3+20) 3 15 (3 + length I.slots))
    , (LevelInfoWin, WinDim 1 (1 + afterBoard) scrW 3)
    , (MessageWin, WinDim 0 (1 + 3 + 1 + afterBoard) scrW 1)
    , (MainWin, WinDim 0 0 scrW scrH)
    , (TutorialWin, WinDim 50 0 3 3) -- Only used by CursesUI, which moves it around
    ]
    where
    -- |Can draw on board at CPos x y for x < bdcW and y < bdcH
    bdcW,bdcH :: Int
    bdcW = B.w * 3 + 1
    bdcH = B.h * 2 + 1
    afterBoard = 3 + max bdcH (3 + length I.slots)
