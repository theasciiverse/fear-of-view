module HighscoreV1 where

import qualified Data.Set  as S

import           Equipment

type Username = String

-- Old version of high score record, preserved so we can load and convert
-- highscore files from old versions of the game
data Highscore = Highscore
    { score     :: Int
    , maxRound  :: Int
    , maxLevel  :: Int
    , name      :: Maybe Username
    , equipment :: S.Set Equipment
    } deriving Eq

type Highscores = [Highscore]
