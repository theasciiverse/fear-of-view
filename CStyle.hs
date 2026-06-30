module CStyle where

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

onBlue, onRed, onMagenta :: ColPair -> ColPair
onBlue = (+8) . (`mod` 8)
onRed = (+16) . (`mod` 8)
onMagenta = (+24) . (`mod` 8)

data CStyle = CStyle { cstyleCol :: ColPair, cstyleBold :: Bool }
style0, styleBold :: CStyle
style0 = CStyle 0 False
styleBold = CStyle 0 True

data Glyph = Glyph { glyphChar :: Char, glyphStyle :: CStyle }

modColour :: (ColPair -> ColPair) -> Glyph -> Glyph
modColour f (Glyph c (CStyle col b)) = Glyph c (CStyle (f col) b)
