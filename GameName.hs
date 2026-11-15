module GameName where

import           System.Directory (XdgDirectory (..), createDirectoryIfMissing,
                                   doesDirectoryExist, getXdgDirectory)

gameName :: String
gameName = "fearOfView"

-- |The folder for the game state and highscores: $XDG_STATE_HOME/fear-of-view,
-- else ~/.local/state/fear-of-view (%LOCALAPPDATA% on Windows). A folder
-- already made by earlier versions ($XDG_DATA_HOME/fearOfView) is kept.
stateDir :: IO FilePath
stateDir = do
    old <- getXdgDirectory XdgData gameName
    oldExists <- doesDirectoryExist old
    dir <- if oldExists then pure old else getXdgDirectory XdgState "fear-of-view"
    createDirectoryIfMissing True dir
    pure dir
