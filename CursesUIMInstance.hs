{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

module CursesUIMInstance where

import           Control.Monad            (forM_)
import           Control.Monad.State
import           Data.List                ((\\))
import           Data.Maybe

import qualified UI.HSCurses.Curses       as C
import qualified UI.HSCurses.CursesHelper as CH

import           CStyle
import           CursesUI

import qualified Command                  as Cmd
import qualified CPos                     as CP
import qualified CursesDraw               as CD
import qualified Game                     as G
import qualified Inventory                as I
import qualified UIMonad                  as UIM

instance UIM.UIMonad UIM where
    runUI m = evalStateT m nullUIState
    initUI = do
        _ <- liftIO $ CH.start >> C.cursSet C.CursorInvisible
        cpairs <- liftIO . colorsToPairs $
            concat [ (,c) <$> cols
                | let cols =
                        [ CH.white, CH.red, CH.green, CH.yellow
                        , CH.blue, CH.magenta, CH.cyan, CH.black]
                , c <- [CH.black,CH.blue,CH.red,CH.yellow]
                ]
        modify $ \s -> s {dispCPairs = cpairs}
        setBkgrnd
        insWin StatusWin =<< liftIO (C.newWin 2 CD.scrW 0 0)
        insWin BoardWin =<< liftIO (C.newWin CD.h (CD.w+1) 3 1)
        insWin InvWin =<< liftIO (C.newWin (3 + length I.slots) 20 3 (1+CD.w+3))
        insWin EquipWin =<< liftIO (C.newWin (3 + length I.slots) 15 3 (1+CD.w+3+20))
        let afterBoard = 3 + max CD.h (3 + length I.slots)
        insWin LevelInfoWin =<< liftIO (C.newWin 3 CD.scrW (1 + afterBoard) 1)
        insWin MessageWin =<< liftIO (C.newWin 1 CD.scrW (1 + 3 + 1 + afterBoard) 0)
        insWin MainWin =<< liftIO (C.newWin CD.scrH CD.scrW 0 0)
        insWin TutorialWin =<< liftIO (C.newWin 3 3 0 50)
        pure True
    endUI = liftIO CH.end
    draw game = unlessSmall $ do
        drawState
        forM_ (allWindows \\ stateWins) wnoutRefresh
        forM_ stateWins wnoutRefresh
        liftIO C.update
        forM_ stateWins wErase
        where
        st = G.playState game
        stateWins = case st of
            G.RoundEnded      -> [MainWin]
            G.Tutorialising b | isJust (CD.tutBox b) -> gameWindows <> [TutorialWin]
            _                 -> gameWindows
        gameWindows = allWindows \\ [MainWin,TutorialWin]
        drawState = case st of
            G.RoundEnded -> CD.drawMainScreen game
            _ -> do
                let bd = G.board game
                forM_ (reverse $ G.transitions game) CD.drawTrans
                CD.drawBoard bd
                CD.drawInv (G.selectedSlot game) (G.preserveSlots game) (G.canGrab game) (G.inventory game) (G.powerOn game)
                CD.drawEquip $ G.equipment game
                CD.drawStatus game
                CD.drawLevelInfo bd
                CD.drawMessage game
                case st of
                    G.Tutorialising b -> CD.highlightTut b
                    _                 -> pure ()
        unlessSmall m = do
            (h,w) <- liftIO C.scrSize
            if h < CD.scrH || w < CD.scrW then
                let s = "Terminal too small!"
                in if w < length s || h < 1 then pure ()
                else do
                    liftIO $ C.erase >> C.refresh
                    drawStr StatusWin style0 (CP.CPos 0 0) s
                    wRefresh StatusWin
            else m
    suspend = do
        liftIO $ CH.suspend >> C.resetParams
        UIM.redraw
    redraw = liftIO $ C.endWin >> C.refresh
    setAsciiOnly a = modify $ \s -> s { asciiOnly = a }
    toggleAsciiOnly = modify $ \s -> s { asciiOnly = not (asciiOnly s) }

    setUIBinding ch cmd = modify $ \s -> s { uiKeyBindings = (ch,cmd) : uiKeyBindings s }

    getChRaw = (charify <$>) $ liftIO $ CH.getKey (pure ()) >>= handleEsc
    getInput = do
        let userResizeCode = 1337  -- XXX: chosen not to conflict with HSCurses codes
        key <- liftIO $ CH.getKey (C.ungetCh userResizeCode) >>= handleEsc
        if key == C.KeyUnknown userResizeCode
            then do
                _ <- liftIO C.scrSize
                pure [Cmd.Redraw]
            else case charify key of
                Just ch -> maybeToList . lookup ch <$> getBindings
                _       -> pure []
