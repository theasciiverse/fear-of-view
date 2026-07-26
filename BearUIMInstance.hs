{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module BearUIMInstance where

import           Control.Monad       (forM_)
import           Control.Monad.State (evalStateT, liftIO, modify)
import           Data.Maybe          (maybeToList)


import qualified BearLibTerminal     as B

import           BearUI
import           CStyle
import           Geometry
import           Window

import qualified CPos                as CP
import qualified Game                as G
import qualified TermDraw            as TD
import qualified UIMonad             as UIM

instance UIM.UIMonad UIM where
    runUI m = evalStateT m nullUIState
    initUI = do
        let wconf = "window: title='Fear of View', size=" <> show scrW <> "x" <> show scrH <> ", resizeable=true;"
            fconf = "font: VeraMoBd.ttf, size=15;"
        b1 <- liftIO B.terminalOpen
        b2 <- liftIO . B.terminalSetString $ wconf
        -- |Try to set font; uses default font if the font file isn't found.
        _ <- liftIO . B.terminalSetString $ fconf
        liftIO $ B.terminalComposition B.CompositionOn
        pure $ b1 && b2
    endUI = liftIO B.terminalClose
    draw game = unlessSmall $ do
        drawTrans
        erase
        drawState
        liftIO B.terminalRefresh
        where
        st = G.playState game
        drawTrans
            | st == G.RoundEnded = pure ()
            | otherwise = do
                wErase BoardWin
                forM_ (reverse $ G.transitions game) TD.drawTrans
        drawState
            | st == G.RoundEnded = TD.drawMainScreen game
            | otherwise = do
                let bd = G.board game
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
            (h,w) <- scrSize
            if h < scrH || w < scrW then
                let s = "Window too small!"
                in if w < length s || h < 1 then pure ()
                else do
                    erase
                    drawStr StatusWin style0 (CP.CPos 0 0) s
                    wRefresh MainWin
            else m
    suspend = pure ()
    redraw = pure ()
    setAsciiOnly a = modify $ \s -> s { asciiOnly = a }
    toggleAsciiOnly = modify $ \s -> s { asciiOnly = not (asciiOnly s) }

    setUIBinding ch cmd = modify $ \s -> s { uiKeyBindings = (ch,cmd) : uiKeyBindings s }

    getChRaw = (charify <$>) . liftIO $ B.terminalRead
    getInput = UIM.getChRaw >>= \case
        Just ch -> maybeToList . lookup ch <$> getBindings
        _       -> pure []
