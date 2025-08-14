{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

module CursesUIMInstance where

import           Control.Monad            (forM_)
import           Control.Monad.State      (evalStateT, liftIO, modify)
import           Data.List                ((\\))
import           Data.Maybe               (isJust, maybeToList)

import qualified Data.Map.Strict          as M
import qualified UI.HSCurses.Curses       as C
import qualified UI.HSCurses.CursesHelper as CH

import           CStyle
import           CursesUI
import           Geometry
import           Window

import qualified Command                  as Cmd
import qualified CPos                     as CP
import qualified Game                     as G
import qualified TermDraw                 as TD
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
        sequence_ [ insWin win =<< liftIO (C.newWin h w y x)
            | (win, WinDim x y w h) <- M.assocs geometry ]
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
            G.Tutorialising b | isJust (TD.tutBox b) -> gameWindows <> [TutorialWin]
            _                 -> gameWindows
        gameWindows = allWindows \\ [MainWin,TutorialWin]
        drawState = case st of
            G.RoundEnded -> TD.drawMainScreen game
            _ -> do
                let bd = G.board game
                forM_ (reverse $ G.transitions game) TD.drawTrans
                TD.drawBoard bd
                TD.drawInv (G.selectedSlot game) (G.preserveSlots game) (G.canGrab game) (G.inventory game) (G.powerOn game)
                TD.drawEquip $ G.equipment game
                TD.drawStatus game
                TD.drawLevelInfo bd
                TD.drawMessage game
                case st of
                    G.Tutorialising b -> TD.highlightTut b
                    _                 -> pure ()
        unlessSmall m = do
            (h,w) <- liftIO C.scrSize
            if h < scrH || w < scrW then
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
