module UIMonad where

import           Control.Exception.Safe
import           Control.Monad.IO.Class

import qualified Command                as Cmd
import qualified Game                   as G

class (Applicative m, MonadIO m, MonadMask m) => UIMonad m where
    runUI :: m a -> IO a
    initUI :: m Bool
    endUI :: m ()
    draw :: G.Game -> m ()
    getInput :: m [ Cmd.Command ]
    getChRaw :: m ( Maybe Char )
    suspend, redraw :: m ()

    setAsciiOnly :: Bool -> m ()
    toggleAsciiOnly :: m ()

    setUIBinding :: Char -> Cmd.Command -> m ()

    doUI :: m a -> IO (Maybe a)
    doUI m = runUI $ do
        ok <- initUI
        if ok then Just <$> m `finally` endUI else pure Nothing
