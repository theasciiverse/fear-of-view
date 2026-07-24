{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

module BearUI where

import           Control.Monad            (void)
import           Control.Monad.State      (StateT, gets, liftIO)
import           Data.Char                (chr, ord)

import qualified BearLibTerminal          as B
import           BearLibTerminal.Keycodes ()
import qualified Data.Map.Strict          as M
import qualified Data.Set                 as S

import           CommonUI
import           CStyle
import           Geometry
import           Window

import qualified CPos                     as CP
import qualified KeyBindings              as KB
import qualified TermM                    as TM

data UIState = UIState
    { uiKeyBindings :: KB.KeyBindings
    , asciiOnly     :: Bool
    }
type UIM = StateT UIState IO
nullUIState :: UIState
nullUIState = UIState [] False

getWin :: Window -> WinDim
getWin win = M.findWithDefault (WinDim 0 0 0 0) win geometry

getBindings :: UIM KB.KeyBindings
getBindings = gets $ (<> KB.defaultBindings) . uiKeyBindings

charify :: B.Keycode -> Maybe Char
charify key = case key of
    B.TkBackspace                -> Just '\b'
    B.TkLeft                     -> Just '←'
    B.TkRight                    -> Just '→'
    B.TkDown                     -> Just '↓'
    B.TkUp                       -> Just '↑'
    B.TkMinus                    -> Just '-'
    B.TkEscape                   -> Just '\ESC'
    B.TkSpace                    -> Just ' '
    B.TkReturn                   -> Just '\r'
    B.TkEnter                    -> Just '\n'
    c | B.TkA <= c && c <= B.TkZ -> Just $ chr (ord 'A' + (fromEnum c - fromEnum B.TkA))
    B.Tk0 -> Just '0'
    c | B.Tk1 <= c && c <= B.Tk9 -> Just $ chr (ord '1' + (fromEnum c - fromEnum B.Tk1))
    B.TkKp0 -> Just '0'
    c | B.TkKp1 <= c && c <= B.TkKp9 -> Just $ chr (ord '1' + (fromEnum c - fromEnum B.TkKp1))
    _                            -> Nothing

wSetStyle :: Window -> CStyle -> UIM ()
wSetStyle _ (CStyle col b) = do
    let (bg,fg) = col `divMod` 8
    liftIO . B.terminalColorUInt $ colour fg b
    liftIO . B.terminalBkColorUInt $ colour (bgTrans bg) False
    where
    bgTrans :: Int -> Int
    bgTrans 1 = 4 -- blue
    bgTrans 2 = 1 -- red
    bgTrans 3 = 5 -- magenta
    bgTrans _ = 7 -- black
    colour c False = case c of
        0 -> 0xffafafaf -- grey
        1 -> 0xffaf0000 -- red
        2 -> 0xff00af00 -- green
        3 -> 0xffafaf00 -- yellow
        4 -> 0xff0000af -- blue
        5 -> 0xffaf008f -- magenta
        6 -> 0xff00afaf -- cyan
        _ -> 0xff000000 -- black
    colour c True = case c of
        0 -> 0xffffffff -- white
        1 -> 0xffff0000 -- bold red
        2 -> 0xff00ff00 -- bold green
        3 -> 0xffffff00 -- bold yellow
        4 -> 0xff0000ff -- bold blue
        5 -> 0xffff00bf -- bold magenta
        6 -> 0xff00ffff -- bold cyan
        _ -> 0xff303030 -- dark grey
withStyle :: Window -> CStyle -> (UIM a -> UIM a)
withStyle w style m = wSetStyle w style >> (m <* wSetStyle w style0)

drawHighlightBoxChars :: CP.CPos -> [Glyph] -> UIM ()
drawHighlightBoxChars p gls = do
    let w = 2 + length gls
    liftIO $ B.terminalLayer 1
    sub <- gets $ subCharAscii . asciiOnly
    withStyle TutorialWin (CStyle magenta True) . liftIO $ drawBorder sub (3,w)
    liftIO $ B.terminalLayer 0
    where
    drawBorder :: (Char -> Char) -> (Int,Int) -> IO ()
    drawBorder sub (h,w) =
        drawBorderWith add w h
        where add y x s = drawStrByChar (p' <> CP.CPos x y) $ sub <$> s
    p' = p <> CP.CPos (-1) (-1)


drawStr :: Window -> CStyle -> CP.CPos -> String -> UIM ()
drawStr w style (CP.CPos x y) s = do
    sub <- gets $ subCharAscii . asciiOnly
    let WinDim dx dy _ _ = getWin w
    withStyle w style . liftIO $ drawStrByChar (CP.CPos (x+dx) (y+dy)) (sub <$> s)

drawStrByChar :: CP.CPos -> String -> IO ()
drawStrByChar (CP.CPos x y) s =
    -- B.terminalPrintString seems to have problems with unicode chars,
    -- so use B.terminalPut instead.
    sequence_ [ B.terminalPut (x+dx) y ch | (dx,ch) <- zip [0..] s ]

drawGlyph :: Window -> CP.CPos -> Glyph -> UIM ()
drawGlyph w (CP.CPos x y) (Glyph ch style) = do
    sub <- gets $ subCharAscii . asciiOnly
    let WinDim dx dy _ _ = getWin w
    withStyle w style . liftIO $ B.terminalPut (x+dx) (y+dy) (sub ch)

drawCompositeGlyph :: Window -> CP.CPos -> CompositeGlyph -> UIM ()
drawCompositeGlyph w p gs =
    mapM_ (drawGlyph w p) $ S.toList gs

erase :: UIM ()
erase = B.terminalClear

wErase, wRefresh :: Window -> UIM ()
wErase win = do
    let WinDim x y w h = getWin win
    void . liftIO $ B.terminalClearArea x y w h

wRefresh _ = liftIO B.terminalRefresh -- no per-win version

instance TM.TermM UIM where
    drawStr = drawStr
    drawGlyph = drawGlyph
    drawCompositeGlyph = drawCompositeGlyph
    wErase = wErase
    wRefresh = wRefresh
    drawHighlightBoxChars = drawHighlightBoxChars
    asciiOnly = gets asciiOnly
