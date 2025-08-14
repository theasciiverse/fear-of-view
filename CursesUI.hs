{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module CursesUI where

import           Control.Exception.Safe
import           Control.Monad.State
import           Data.Char              (chr)
import           Foreign.Ptr

import qualified Data.Map.Strict        as M
import qualified UI.HSCurses.Curses     as C

import           CStyle
import           Window

import qualified CPos                   as CP
import qualified KeyBindings            as KB
import qualified TermM                  as TM

data UIState = UIState
    { dispCPairs    :: [C.Pair]
    , uiKeyBindings :: KB.KeyBindings
    , windows       :: M.Map Window C.Window
    , asciiOnly     :: Bool
    }
type UIM = StateT UIState IO
nullUIState :: UIState
nullUIState = UIState [] [] M.empty False

insWin :: Window -> C.Window -> UIM ()
insWin w cw = do
    modify $ \s -> s {windows = M.insert w cw $ windows s}
    wsetBkgrnd w

getWin :: Window -> UIM C.Window
getWin w = gets $ M.findWithDefault nullPtr w . windows

getBindings :: UIM KB.KeyBindings
getBindings = gets $ (<> KB.defaultBindings) . uiKeyBindings

charify :: C.Key -> Maybe Char
charify key = case key of
    C.KeyChar ch   -> Just ch
    C.KeyBackspace -> Just '\b'
    C.KeyLeft      -> Just '←'
    C.KeyRight     -> Just '→'
    C.KeyDown      -> Just '↓'
    C.KeyUp        -> Just '↑'
    _              -> Nothing

handleEsc :: C.Key -> IO C.Key
handleEsc k@(C.KeyChar '\ESC') = do
    C.timeout 100
    cch <- C.getch
    C.timeout (-1)
    pure $ if cch == -1 then k
        else C.KeyChar $ chr $ fromIntegral cch+128
handleEsc k = pure k

-- From Curses.CursesHelper:
-- | Converts a list of 'Curses.Color' pairs (foreground color and
--   background color) into the curses representation 'Curses.Pair'.
colorsToPairs :: [(C.Color, C.Color)] -> IO [C.Pair]
colorsToPairs cs = do
    p <- C.colorPairs
    let nColors = length cs
        blackWhite = p < nColors
    if blackWhite then pure $ replicate nColors $ C.Pair 0
        -- print ("Terminal does not support enough colors. Number of " <>
        --          " colors requested: " <> show nColors <>
        --          ". Number of colors supported: " <> show p)
    else mapM toPairs (zip [1..] cs)
    where toPairs (n, (fg, bg)) = do
            let p = C.Pair n
            C.initPair p fg bg
            pure p

setBkgrnd :: UIM ()
setBkgrnd = do
    cpairs <- gets dispCPairs
    liftIO . C.bkgrndSet a0 $ cpairs !! white
    liftIO $ C.erase >> C.refresh

wsetBkgrnd :: Window -> UIM ()
-- >= hscurses-1.5.1 required for C.wbkgrndSet
#if MIN_VERSION_hscurses(1,5,1)
wsetBkgrnd w = do
    cpairs <- gets dispCPairs
    cw <- getWin w
    liftIO . C.wbkgrndSet cw a0 $ cpairs !! white
    liftIO $ C.werase cw >> C.wRefresh cw
#else
wsetBkgrnd _ = pure ()
#endif

wSetStyle :: Window -> CStyle -> UIM ()
wSetStyle w (CStyle col attr) = do
    cpairs <- gets dispCPairs
    cw <- getWin w
    liftIO $ C.wAttrSet cw (attr, cpairs!!col)
withStyle :: Window -> CStyle -> (UIM a -> UIM a)
withStyle w style m = wSetStyle w style >> (m <* wSetStyle w style0)

subCharAscii :: Bool -> Char -> Char
subCharAscii True = \case
    '·' -> '+'
    '┌' -> '+'
    '┐' -> '+'
    '└' -> '+'
    '┘' -> '+'
    '│' -> '|'
    '║' -> '}'
    '─' -> '-'
    '═' -> '='
    c   -> c
subCharAscii False = id

drawHighlightBoxChars :: CP.CPos -> [Glyph] -> UIM ()
drawHighlightBoxChars (CP.CPos x y) gls = do
    -- hscurses doesn't expose mvwin, so manually recreate the window in the
    -- right position:
    wnoutRefresh TutorialWin
    liftIO . C.delWin =<< getWin TutorialWin
    let w = 2 + length gls
    insWin TutorialWin =<< liftIO (C.newWin 3 w (y-1) (x-1))
    ascii <- gets asciiOnly
    withStyle TutorialWin (CStyle magenta aBold) $ liftIO . drawBorder ascii (3,w) =<< getWin TutorialWin
    sequence_ [ drawGlyph TutorialWin (CP.CPos (1+n) 1) gl | (gl,n) <- zip gls [0..] ]
    where
    -- Draw border manually rather than using C.wBorder:
    -- default border characters are ugly on e.g. windows PuTTY,
    -- and trying to set C.Border to use box-drawing chars doesn't work.
    drawBorder :: Bool -> (Int,Int) -> C.Window -> IO ()
    drawBorder ascii (h,w) cw = do
        let add yy xx = C.mvWAddStr cw yy xx . (subCharAscii ascii <$>)
        add 0 0 $ '┌':replicate (w-2) '─' <> "┐"
        sequence_ [ add y' 0 "│" >> add y' (w-1) "│"
            | y' <- [1..h-2] ]
        -- |This throws an error due to moving cursor out of the window
        add (h-1) 0 ('└':replicate (w-2) '─' <> "┘")
            `catchIO` (\_ -> pure ())

drawStr :: Window -> CStyle -> CP.CPos -> String -> UIM ()
drawStr w style (CP.CPos x y) s = do
    ascii <- gets asciiOnly
    cw <- getWin w
    withStyle w style . liftIO $ C.mvWAddStr cw y x (subCharAscii ascii <$> s)

drawGlyph :: Window -> CP.CPos -> Glyph -> UIM ()
drawGlyph w p (Glyph ch style) = drawStr w style p $ ch:""

wErase, wRefresh, wnoutRefresh :: Window -> UIM ()
wErase w = liftIO . C.werase =<< getWin w
wRefresh w = liftIO . C.wRefresh =<< getWin w
wnoutRefresh w = liftIO . C.wnoutRefresh =<< getWin w

instance TM.TermM UIM where
    drawStr = drawStr
    drawGlyph = drawGlyph
    wErase = wErase
    wRefresh = wRefresh
    drawHighlightBoxChars = drawHighlightBoxChars
    asciiOnly = gets asciiOnly

