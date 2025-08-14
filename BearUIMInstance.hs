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
import           Geometry

import qualified Game                as G
import qualified TermDraw            as TD
import qualified UIMonad             as UIM

instance UIM.UIMonad UIM where
    runUI m = evalStateT m nullUIState
    initUI = do
        let wconf = "window: title='Fear of View', size=" <> show scrW <> "x" <> show scrH <> ";"
            fconf = "font: VeraMoBd.ttf, size=15;"
        b1 <- liftIO B.terminalOpen
        b2 <- liftIO . B.terminalSetString $ wconf
        -- |Try to set font; uses default font if the font file isn't found.
        _ <- liftIO . B.terminalSetString $ fconf
        pure $ b1 && b2
    endUI = liftIO B.terminalClose
    draw game = do
        erase
        drawState
        liftIO B.terminalRefresh
        where
        st = G.playState game
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
    suspend = pure ()
    redraw = pure ()
    setAsciiOnly a = modify $ \s -> s { asciiOnly = a }
    toggleAsciiOnly = modify $ \s -> s { asciiOnly = not (asciiOnly s) }

    setUIBinding ch cmd = modify $ \s -> s { uiKeyBindings = (ch,cmd) : uiKeyBindings s }

    getChRaw = (charify <$>) . liftIO $ B.terminalRead
    getInput = UIM.getChRaw >>= \case
        Just ch -> maybeToList . lookup ch <$> getBindings
        _       -> pure []
