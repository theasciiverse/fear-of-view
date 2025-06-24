module HighscoreFile where

import           Codec.Serialise
import           Control.Exception.Safe
import           Control.Monad
import           Data.Either
import           System.Directory
import           System.FileLock        (SharedExclusive (..), withFileLock)
import           System.FilePath        ((</>))

import           GameName
import qualified Highscore              as HS
import           Serialise              ()

getPath :: IO FilePath
getPath = do
    dir <- getXdgDirectory XdgData gameName
    createDirectoryIfMissing True dir
    pure $ dir </> "highscores"

add :: HS.Highscore -> IO ()
add hs = do
    path <- getPath
    withFileLock (path<>".lock") Exclusive $ \_ -> do
        hss <- (fromRight HS.empty <$>) . tryAny $ readFileDeserialise path
        void . tryAny . writeFileSerialise path $ HS.add hs hss

get :: IO HS.Highscores
get = do
    path <- getPath
    (fromRight HS.empty <$>) . tryAny . withFileLock (path<>".lock") Shared $ \_ -> readFileDeserialise path
