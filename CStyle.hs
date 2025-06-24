module CStyle where

import qualified UI.HSCurses.Curses as C

type ColPair = Int
white,red,green,yellow,blue,magenta,cyan,black :: ColPair
white = 0
red = 1
green = 2
yellow = 3
blue = 4
magenta = 5
cyan = 6
black = 7

onBlue, onRed, onYellow :: ColPair -> ColPair
onBlue = (+8) . (`mod` 8)
onRed = (+16) . (`mod` 8)
onYellow = (+24) . (`mod` 8)

data CStyle = CStyle { cstyleCol :: ColPair, cstyleAttr :: C.Attr }
a0, aBold :: C.Attr
a0 = C.attr0
aBold = C.setBold a0 True
style0, styleBold :: CStyle
style0 = CStyle 0 a0
styleBold = CStyle 0 aBold

data Glyph = Glyph { glyphChar :: Char, glyphStyle :: CStyle }

modColour :: (ColPair -> ColPair) -> Glyph -> Glyph
modColour f (Glyph c (CStyle col a)) = Glyph c (CStyle (f col) a)
