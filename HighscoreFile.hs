module HighscoreFile (add, get) where

import           Codec.Serialise
import           Control.Exception.Safe
import           Control.Monad
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

readHSs :: String -> IO HS.Highscores
readHSs path =
    handleAny (const tryV1) $ readFileDeserialise path
    where
    tryV1 = (either (const HS.empty) (HS.fromV1 <$>) <$>) . tryAny $ readFileDeserialise path

add :: HS.Highscore -> IO ()
add hs = do
    path <- getPath
    withFileLock (path<>".lock") Exclusive $ \_ ->
        (void . tryAny . writeFileSerialise path) . HS.add hs =<< readHSs path

get :: IO HS.Highscores
get = do
    path <- getPath
    withFileLock (path<>".lock") Shared $ \_ ->
        readHSs path
