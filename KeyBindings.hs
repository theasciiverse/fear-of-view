module KeyBindings (KeyBindings, defaultBindings, findBindings, findBinding, showKey,
    showKeyChar, showKeyFriendly, showKeyFriendlyShort, dvorakViBindings) where

import           Data.Bits  (xor)
import           Data.Char
import           Data.List
import           Data.Maybe

import qualified Command    as C
import qualified Inventory  as I
import qualified Pos        as P

type KeyBindings = [ (Char,C.Command) ]

ctrl, unctrl, meta, unmeta :: Char -> Char
ctrl = toEnum . xor 64 . fromEnum
meta = toEnum . xor 128 . fromEnum
unctrl = ctrl
unmeta = meta

lowerToo :: KeyBindings -> KeyBindings
lowerToo = concatMap addLower
    where addLower b@(c, cmd) = [ b, (toLower c, cmd) ]

quitBindings, qwertyViBindings, wasdBindings, dvorakViBindings, cursorBindings, actionBindings, basicBindings, defaultBindings :: KeyBindings
quitBindings = lowerToo
    [ ('Q', C.Quit)
    , (ctrl '[', C.Quit)
    , (ctrl 'C', C.Quit) ]

qwertyViBindings = lowerToo
    [ ('H', C.Dir P.DLeft)
    , ('J', C.Dir P.DDown)
    , ('K', C.Dir P.DUp)
    , ('L', C.Dir P.DRight)
    ]

dvorakViBindings =
    [ ('h', C.Dir P.DLeft)
    , ('t', C.Dir P.DDown)
    , ('n', C.Dir P.DUp)
    , ('s', C.Dir P.DRight)
    ]

wasdBindings = lowerToo
    [ ('A', C.Dir P.DLeft)
    , ('Z', C.Dir P.DLeft)  -- for AZERTY
    , ('S', C.Dir P.DDown)
    , ('W', C.Dir P.DUp)
    , ('D', C.Dir P.DRight)
    -- |for dvorak
    , ('E', C.Dir P.DRight)
    , ('O', C.Dir P.DDown)
    , (',', C.Dir P.DUp)
    , ('<', C.Dir P.DUp)
    ]

cursorBindings =
    [ ('←', C.Dir P.DLeft)
    , ('↓', C.Dir P.DDown)
    , ('↑', C.Dir P.DUp)
    , ('→', C.Dir P.DRight)
    ]

actionBindings = [ (chr $ ord '1' + (slot-1), C.UseInv slot) | slot <- I.slots ]
    <> [ ('0', C.UsePower) ]

basicBindings =
    [ (' ', C.Accept)
    , ('\r', C.Accept)
    , ('\n', C.Accept)
    , ('\f', C.Redraw)
    -- XXX: disabling suspend, because it seems to be broken for no apparent
    -- reason.
    --, (ctrl 'Z', C.Suspend)
    , ('t', C.SkipTutorial)
    , ('T', C.SkipTutorial)
    , ('-', C.ToggleAscii)
    ]

debugBindings :: KeyBindings
debugBindings =
    [ (ctrl 'P', C.DebugAddPower)
    , (ctrl 'J', C.DebugAddJunk)
    , (ctrl 'I', C.DebugAddItems)
    , (ctrl 'E', C.DebugExit)
    ]

defaultBindings = quitBindings <> debugBindings <> wasdBindings <> qwertyViBindings <> cursorBindings <> actionBindings <> basicBindings

findBindings :: KeyBindings -> C.Command -> [Char]
findBindings bdgs cmd = nub
    $ [ ch | (ch,cmd') <- bdgs, cmd'==cmd ]

findBinding :: KeyBindings -> C.Command -> Maybe Char
findBinding = (listToMaybe.) . findBindings

showKey :: Char -> String
showKey ch
    | isAscii (unmeta ch) = 'M':'-':showKey (unmeta ch)
    | isPrint ch = [ch]
    | isPrint (unctrl ch) = '^':[unctrl ch]
    | otherwise = "[?]"

showKeyFriendly, showKeyFriendlyShort :: Char -> String
showKeyFriendly ' '  = "space"
showKeyFriendly '\r' = "return"
showKeyFriendly '\n' = "newline"
showKeyFriendly '\t' = "tab"
showKeyFriendly '\b' = "bksp"
showKeyFriendly ch   = showKey ch

showKeyFriendlyShort '\r' = "ret"
showKeyFriendlyShort '\t' = "tab"
showKeyFriendlyShort '\b' = "bksp"
showKeyFriendlyShort ch   = showKey ch

showKeyChar :: Char -> Char
showKeyChar ch
    | isAscii (unmeta ch) = '['
    | isPrint ch = ch
    | isPrint (unctrl ch) = '^'
    | otherwise = '?'
