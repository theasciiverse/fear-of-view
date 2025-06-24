module CPos where

-- | curses convention (y increases downwards)
data CPos = CPos { x :: Int, y :: Int } deriving (Eq, Ord)
instance Semigroup CPos where
    CPos x1 y1 <> CPos x2 y2 = CPos (x1+x2) (y1+y2)
instance Monoid CPos where
    mempty = CPos 0 0

