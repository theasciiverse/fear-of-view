module TermM where

import           Control.Monad.IO.Class (MonadIO)

import           CStyle
import           Window

import qualified CPos                   as CP

class MonadIO m => TermM m where
    drawStr :: Window -> CStyle -> CP.CPos -> String -> m ()
    drawGlyph :: Window -> CP.CPos -> Glyph -> m ()
    wErase, wRefresh :: Window -> m ()
    drawHighlightBoxChars :: CP.CPos -> [Glyph] -> m ()
    asciiOnly :: m Bool
